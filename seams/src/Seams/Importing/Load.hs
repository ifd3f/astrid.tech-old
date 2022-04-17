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
         runExceptT (performEnvRead asYaml BadConfig p))
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
  dContent <- performEnvRead asFile (NoDocument path) path
  liftEither $
    case parseYamlFrontmatter dContent of
      Fail _ _ err -> Left $ BadYaml err
      Partial _ -> Left IncompleteFrontmatter
      Done rest fm -> Right $ LoadedDoc path fm (Content path Markdown rest)
loadDocAsType YAML path = LoadedDoc path <$> yaml <*> content
  where
    yaml = performEnvRead asYaml (NoDocument path) path
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
    fileContent = performEnvRead asFile (NoContent path) path

performEnvRead ::
     Monad m
  => (ReadResult ByteString -> Maybe a)
  -> LoadError
  -> FilePath
  -> DocLoaderT m a
performEnvRead sel err path = do
  result <- lift $ envRead path
  case sel result of
    Just d -> pure d
    Nothing -> throwError err

asFile :: ReadResult a -> Maybe a
asFile (File f) = Just f
asFile _ = Nothing

asDir :: ReadResult a -> Maybe [String]
asDir (Dir c) = Just c
asDir _ = Nothing

asYaml :: FromJSON a => ReadResult ByteString -> Maybe a
asYaml (File f) = either (const Nothing) Just $ decodeEither' f
asYaml _ = Nothing
