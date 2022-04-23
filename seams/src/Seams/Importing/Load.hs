{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Seams.Importing.Load where

import Control.Monad.Except
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Either.Combinators
import Data.Either.Utils (maybeToEither)
import Data.Frontmatter
import Data.Functor ((<&>))
import Data.Maybe
import Data.Validation
import Data.Yaml.Aeson (decodeEither')
import Seams.Importing.ReadFile
import Seams.Importing.Types
import Seams.Types
import System.FilePath

type ContentLoaderT m a = ReadFileT' m a

type DocLoaderT m a = ExceptT LoadError (ReadFileT' m) a

loadContentFolder ::
     Monad m
  => FilePath
  -> ContentLoaderT m (Validation [WithPath LoadError] LoadedContent)
loadContentFolder path = do
  blogResults <- loadDocs (path </> "blog")
  projectResults <- loadDocs (path </> "projects")
  tagResults <- loadMergeableDir (path </> "tags")
  pure $ LoadedContent <$> blogResults <*> projectResults <*> tagResults

loadDocs ::
     (Monad m, FromJSON d)
  => FilePath
  -> ContentLoaderT m (Validation [WithPath LoadError] [LoadedDoc d])
loadDocs path = do
  files <- envWalkFiles path
  let loadable = mapMaybe loadDocument' files
  results <- sequenceA loadable
  pure $ sequenceA results

loadMergeableDir ::
     (Monad m, FromJSON a, Monoid a)
  => FilePath
  -> ContentLoaderT m (Validation [WithPath LoadError] a)
loadMergeableDir path = do
  files <- envWalkFiles path
  results <-
    traverse
      (\p ->
         fromEither . mapLeft (\e -> [WithPath p e]) <$>
         runExceptT (performEnvRead (asYaml BadConfig) p))
      files
  return $ mconcat <$> sequenceA results

loadDocument' ::
     (Monad m, FromJSON d)
  => FilePath
  -> Maybe (ReadFileT' m (Validation [WithPath LoadError] (LoadedDoc d)))
loadDocument' path =
  loadDocument path <&>
  (\(WithPath p loader) -> do
     result <- runExceptT loader
     pure $ fromEither $ mapLeft (\e -> [WithPath p e]) result)

loadDocument ::
     (Monad m, FromJSON d)
  => FilePath
  -> Maybe (WithPath (DocLoaderT m (LoadedDoc d)))
loadDocument path = do
  dType <- extensionToDocumentType $ takeExtension path
  return $ WithPath path (loadDocAsType dType path)

loadDocAsType ::
     (FromJSON d, Monad m)
  => DocumentType
  -> FilePath
  -> DocLoaderT m (LoadedDoc d)
loadDocAsType FrontmatterMarkdown path = do
  dContent <- performEnvRead (asFile $ NoDocument FrontmatterMarkdown path) path
  liftEither $
    case parseYamlFrontmatter dContent of
      Fail _ _ err -> Left $ BadYaml err
      Partial _ -> Left IncompleteFrontmatter
      Done rest fm -> Right $ LoadedDoc path fm (Content path Markdown rest)
loadDocAsType YAML path = LoadedDoc path <$> yaml <*> content
  where
    yaml = performEnvRead (asYaml $ NoDocument YAML path) path
    content = loadContent (takeBaseName path)

loadContent :: Monad m => FilePath -> DocLoaderT m Content
loadContent path = Content path <$> cType <*> fileContent
  where
    ext = takeExtension path
    cType =
      liftEither $
      maybeToEither
        (UnsupportedContentExtension ext)
        (extensionToContentType ext)
    fileContent = performEnvRead (asFile $ NoContent path) path

performEnvRead ::
     Monad m
  => (ReadResult ByteString -> Either LoadError a)
  -> FilePath
  -> DocLoaderT m a
performEnvRead sel path = do
  result <- lift $ envRead path
  liftEither $ sel result

asFile :: LoadError -> ReadResult a -> Either LoadError a
asFile _ (File f) = Right f
asFile e _ = Left e

asDir :: LoadError -> ReadResult a -> Either LoadError [String]
asDir _ (Dir c) = Right c
asDir e _ = Left e

asYaml :: FromJSON a => LoadError -> ReadResult ByteString -> Either LoadError a
asYaml _ (File f) = mapLeft BadYaml' $ decodeEither' f
asYaml e _ = Left e
