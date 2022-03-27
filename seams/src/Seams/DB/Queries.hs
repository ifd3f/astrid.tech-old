{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Seams.DB.Queries where
import Database.Persist
import qualified Seams.DB.Models as DB
import Seams.Importing.Types
import Data.Foldable
import Control.Lens
import Seams.Importing.FileSchema
import Data.Time
import qualified Data.Map as M

insertOrGetTagId slug = either entityKey id <$> insertBy (DB.Tag slug Nothing Nothing Nothing)

insertTagObjWithSlugs tagSlugs = do
  obj <- insert DB.TagObj
  tags <- traverse insertOrGetTagId tagSlugs
  let assocs = zipWith (DB.TagAssoc obj) [0..] tags
  traverse_ insert assocs
  return obj

insertContent content = insert $ DB.Content
  (content^.contentPath)
  (content^.contentType)
  (content^.contentBody)

insertDoc (LoadedDoc path doc content) = do
  contentObj <- insert $ DB.Content
    (content^.contentPath)
    (content^.contentType)
    (content^.contentBody)

  insert $ DB.Doc
    contentObj
    path
    (zonedTimeToUTC $ doc^.docTime.tsPublished)
    (zonedTimeToUTC <$> doc^.docTime.tsCreated)
    (zonedTimeToUTC <$> doc^.docTime.tsModified)

insertPost ld = do
  tagObj <- insertTagObjWithSlugs $ extra^.postTags
  docObj <- insertDoc ld
  insert $ DB.Post
    (extra^.postTitle)
    (extra^.postTagline)
    (extra^.postSlug.slugYear)
    (extra^.postSlug.slugMonth)
    (extra^.postSlug.slugDay)
    (extra^.postSlug.slugOrdinal)
    (extra^.postSlug.slugName)
    docObj
    tagObj
  where extra = ld^.ldMeta.docExtra

insertProject ld = do
  tagObj <- insertTagObjWithSlugs $ extra^.projectTags
  docObj <- insertDoc ld
  insert $ DB.Project
    (extra^.projectTitle)
    (extra^.projectTagline)
    (extra^.projectSlug)
    (zonedTimeToUTC $ extra^.projectStart)
    docObj
    tagObj
  where extra = ld^.ldMeta.docExtra

insertTagTitle slug title = upsert
  (DB.Tag slug (Just title) Nothing Nothing)
  [DB.TagTitle =. Just title]

insertTagColorSheet (TagColorSheet text bg tags) = traverse_ (\slug ->
  upsert
    (DB.Tag slug Nothing text bg)
    [DB.TagTextColor =. text, DB.TagBgColor =. bg])
  tags

insertTagConfig (TagConfig titles colors) = do
  traverse_ (uncurry insertTagTitle) $ M.assocs titles
  insertTagColorSheet colors

