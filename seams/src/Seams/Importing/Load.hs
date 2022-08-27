{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Seams.Importing.Load where

import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.Either.Combinators
import Data.Either.Utils (maybeToEither)
import Data.Frontmatter
import Data.Maybe
import Data.Validation (Validation(..), fromEither)
import Data.Yaml.Aeson (decodeEither')
import Seams.Importing.FileSchema
import Seams.Importing.ReadFile
import Seams.Importing.Types
import Seams.Types
import System.FilePath

type ContentLoaderT m a = ReadFileT ByteString m a

type DocLoaderT m a = ExceptT LoadError (ReadFileT ByteString m) a

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
  loadResults <- traverse (runExceptT . loadDocument) files
  let documents =
        mapMaybe
          (\case
             (Right d, _) -> Just (Right d)
             (Left (UnsupportedDocumentExtension _), _) -> Nothing
             (Left (NoDocument _ _), _) -> Nothing
             (Left err, p) -> Just (Left (WithPath p err)))
          (zip loadResults files)
  pure $
    case partitionEithers documents of
      ([], vals) -> Success vals
      (errs, _) -> Failure errs

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

-- | Load a document at the path, detecting its content.
loadDocument :: (Monad m, FromJSON d) => FilePath -> DocLoaderT m (LoadedDoc d)
loadDocument path = do
  dType <-
    case extensionToDocumentType ext of
      Nothing -> throwError $ UnsupportedDocumentExtension ext
      Just t -> pure t
  loadDocAsType dType path
  where
    ext = takeExtension path

-- | Load a document at the path with the given type, plus its content.
loadDocAsType ::
     (FromJSON d, Monad m)
  => DocumentType
  -> FilePath
  -> DocLoaderT m (LoadedDoc d)
loadDocAsType FrontmatterMarkdown path = do
  dContent <- performEnvRead (asFile $ NoDocument FrontmatterMarkdown path) path
  case parseYamlFrontmatter dContent of
    Fail _ _ "string" -> throwError $ NoDocument FrontmatterMarkdown path
    Fail _ _ err -> throwError $ BadYaml err
    Partial _ -> throwError IncompleteFrontmatter
    Done rest fm -> pure $ LoadedDoc path fm (Content path Markdown rest)
loadDocAsType YAML path = do
  yaml :: Doc d <- performEnvRead (asYaml $ NoDocument YAML path) path
  content <-
    case yaml ^. docContent of
      Just (EmbeddedPlaintext x) -> pure $ Content path Plaintext x
      Just (FileRef ref) -> loadContent ref
      Nothing -> loadContent $ dropExtension path
  pure $ LoadedDoc path yaml content

-- | Load a content object from the given path.
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
