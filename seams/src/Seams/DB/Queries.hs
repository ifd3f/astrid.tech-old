{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Seams.DB.Queries where
import Database.Persist
import qualified Seams.DB.Models as DB
import Data.Sequence
import Data.Foldable

insertOrGetTagId slug = either entityKey id <$> insertBy (DB.Tag slug Nothing)

insertTagObjWithSlugs tagSlugs = do
  obj <- insert DB.TagObj
  tags <- traverse insertOrGetTagId tagSlugs
  let assocs = mapWithIndex (DB.TagAssoc obj) tags
  traverse_ insert assocs
  return obj

insertDoc doc content = do
  content <- insert $ DB.Content 
  
