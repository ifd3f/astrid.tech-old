{-# LANGUAGE ScopedTypeVariables #-}

module Seams.Importing.Load where

import System.FilePath
import Data.Aeson
import Data.Frontmatter
import Data.Yaml
import Data.Either.Combinators
import Data.Either.Utils
import Seams.Importing.ReadFile
import Seams.Importing.Types
import Seams.Types

newtype LoadError = LoadError String

loadDocument :: (MonadFail m, FromJSON h) => FilePath -> ReadFileT m (LoadedDoc h)
loadDocument path = case extensionToDocumentType $ takeExtension path of
  Just dType -> loadDocAsType dType path
  Nothing -> fail $ "unknown doctype at path " ++ path

loadDocAsType :: (MonadFail m, FromJSON h) => DocumentType -> FilePath -> ReadFileT m (LoadedDoc h)
loadDocAsType YAML path =
  let
    contentPath = takeBaseName path
    contentExt = takeExtension contentPath
  in do
    dContent <- envReadFile path

    (doc, contentType) <- either fail pure $ do
      doc <- mapLeft show $ decodeEither' dContent
      contentType <- maybeToEither ("unsupported content type at " ++ contentPath) $ extensionToContentType contentExt
      pure (doc, contentType)

    contentBody <- envReadFile contentPath
    pure $ LoadedDoc path doc (Content contentPath contentType contentBody)

loadDocAsType FrontmatterMarkdown path = do
  dContent <- envReadFile path
  case parseYamlFrontmatter dContent of
    Fail _ _ err -> fail err
    Partial _ -> fail "Incomplete input"
    Done rest fm -> pure $ LoadedDoc path fm (Content path Markdown rest)

