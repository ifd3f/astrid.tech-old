module Seams.DB where
import Seams.InputSchema
import Database.PostgreSQL.Typed.Query

insertPost :: Doc PostMeta -> PGSimpleQuery
insertPost doc = [pgSQL|
  INSERT INTO posts
  VALUES (
  )|]

