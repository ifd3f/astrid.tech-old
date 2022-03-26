{-# LANGUAGE ScopedTypeVariables #-}

module Seams.Importing.Load where

import System.FilePath
import Seams.Importing.FileSchema
import Data.ByteString (ByteString)
import Control.Monad.Trans.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Frontmatter
import Data.Yaml
import qualified Data.Yaml as Yaml
import Control.Monad.Trans.Except
import Control.Arrow
import Data.Either.Combinators
import Data.Either.Utils
import Data.Functor.Identity 
import Seams.Importing.ReadFile
import Seams.Importing.Types

newtype LoadError = LoadError String

loadDocument path = case extensionToDocumentType $ takeExtension path of
  Just dType -> loadDocAsType dType path
  Nothing -> fail $ "unknown doctype at path " ++ path

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
    pure $ Document path doc (Content contentPath contentType contentBody)

loadDocAsType FrontmatterMarkdown path = do
  dContent <- envReadFile path
  case parseYamlFrontmatter dContent of
    Fail _ _ err -> fail err
    Partial _ -> fail "Incomplete input"
    Done rest fm -> pure $ Document path fm (Content path Markdown rest)

