{-# LANGUAGE ScopedTypeVariables #-}

module Seams.Importing.Load where

import System.FilePath
import Data.Aeson
import Data.Frontmatter
import Data.Either.Utils
import Seams.Importing.ReadFile
import Seams.Importing.Types
import Seams.Types
import Data.Maybe

newtype LoadError = LoadError String

loadContentFolder :: (MonadFail m) => ReadFileT' m LoadedContent
loadContentFolder = LoadedContent <$>
  loadDocs "blog" <*>
  loadDocs "projects" <*>
  loadMergeableDir "tags"

loadDocs :: (MonadFail m, FromJSON h) => FilePath -> ReadFileT' m [LoadedDoc h]
loadDocs path = do
  files <- envWalkFiles path
  documents <- traverse loadDocument files
  return $ catMaybes documents

loadMergeableDir :: (MonadFail m, FromJSON a, Monoid a) => FilePath -> ReadFileT' m a
loadMergeableDir path = do
  files <- envWalkFiles path
  documents <- traverse envReadYAML files
  return $ mconcat documents

loadDocument :: (MonadFail m, FromJSON h) => FilePath -> ReadFileT' m (Maybe (LoadedDoc h))
loadDocument path = case extensionToDocumentType $ takeExtension path of
  Just dType -> Just <$> loadDocAsType dType path
  Nothing -> pure Nothing

loadDocAsType :: (MonadFail m, FromJSON h) => DocumentType -> FilePath -> ReadFileT' m (LoadedDoc h)
loadDocAsType YAML path = LoadedDoc path <$>
  envReadYAML path <*>
  loadContent (takeBaseName path)

loadDocAsType FrontmatterMarkdown path = do
  dContent <- envReadFile path
  case parseYamlFrontmatter dContent of
    Fail _ _ err -> fail err
    Partial _ -> fail "Incomplete input"
    Done rest fm -> pure $ LoadedDoc path fm (Content path Markdown rest)

loadContent :: MonadFail m => FilePath -> ReadFileT' m Content
loadContent path = do
  cType <- either fail pure $ maybeToEither
    ("unsupported content type at " ++ path) $
    extensionToContentType $ takeExtension path
  body <- envReadFile path
  return $ Content path cType body

