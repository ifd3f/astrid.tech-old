{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Seams.Importing.Load where

import System.FilePath
import Data.Aeson
import Data.Frontmatter
import Seams.Importing.ReadFile
import Seams.Importing.Types
import Seams.Types
import Data.Maybe
import Data.Functor
import Data.Either.Utils ( maybeToEither )
import Control.Monad.Except
import Data.Either.Combinators
import Data.Validation

-- loadContentFolder :: FilePath -> ReadFileT' m (DirImportResult LoadedContent)
loadContentFolder path = LoadedContent <$>
  loadDocs (path </> "blog") <*>
  loadDocs (path </> "projects") <*>
  loadMergeableDir (path </> "tags")

loadDocs :: FromJSON h => FilePath -> ReadFileT' m (DirImportResult [LoadedDoc h])
loadDocs path = mapMaybe loadDocument <$> envWalkFiles path

loadMergeableDir :: (FromJSON a, Monoid a) => FilePath -> ReadFileT' m (Validation [WithPath LoadError] a)
loadMergeableDir path = do
  files <- envWalkFiles path
  results <- traverse (\p -> fromEither . mapLeft (\e -> [WithPath p e]) <$> readYamlLE (BadConfig p) p) files
  return $ mconcat <$> sequenceA results

loadDocument :: (Functor m1, FromJSON d) => FilePath -> Maybe (ReadFileT' m1 (WithPath (Either LoadError (LoadedDoc d))))
loadDocument path = do
  dType <- extensionToDocumentType $ takeExtension path
  return $ WithPath path <$> loadDocAsType dType path

loadDocAsType :: (FromJSON d) => DocumentType -> FilePath -> ReadFileT' m1 (Either LoadError (LoadedDoc d))
loadDocAsType FrontmatterMarkdown path = runExceptT $ do
  dContent <- envRead' asFile (NoDocument path) path
  liftEither $ case parseYamlFrontmatter dContent of
    Fail _ _ err -> Left $ BadYaml err
    Partial _ -> Left IncompleteFrontmatter
    Done rest fm -> Right $ LoadedDoc path fm (Content path Markdown rest)

loadDocAsType YAML path = runExceptT $ LoadedDoc path <$> yaml <*> content
  where
    yaml = readYamlLE' (NoDocument path) path
    content = ExceptT (loadContent (takeBaseName path))

loadContent :: Functor m => FilePath -> ReadFileT' m (Either LoadError Content)
loadContent path = runExceptT $ Content path <$> cType <*> fileContent
  where
    ext = takeExtension path
    cType = liftEither $ maybeToEither (UnsupportedContentExtension ext) (extensionToContentType ext)
    fileContent = envRead' asFile (NoContent path) path

readYamlLE :: (FromJSON b, Monad m) => LoadError -> FilePath -> ReadFileT' m (Either LoadError b)
readYamlLE err path = mapLeft (maybe err BadYaml') <$> envReadYAML path

readYamlLE' :: (FromJSON a, Monad m) => LoadError -> FilePath -> ExceptT LoadError (ReadFileT' m) a
readYamlLE' err path = ExceptT $ readYamlLE err path

