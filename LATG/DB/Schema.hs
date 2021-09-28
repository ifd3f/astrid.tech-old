-- oh god why
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module LATG.DB.Schema where

import Data.Profunctor.Product (p10, p13, p15)
import Opaleye
  ( Field,
    FieldNullable,
    SqlArray,
    SqlInt2,
    SqlInt4,
    SqlText,
    SqlTimestamptz,
    SqlUuid,
    Table,
    optionalTableField,
    readOnlyTableField,
    requiredTableField,
    table,
  )

type DocumentsTable = Table DocumentsW DocumentsR

type DocumentsW =
  ( (),
    Field SqlUuid,
    Field SqlTimestamptz,
    Field SqlTimestamptz,
    Maybe (FieldNullable SqlTimestamptz),
    Field SqlText,
    Field SqlText,
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText)
  )

type DocumentsR =
  ( Field SqlInt4,
    Field SqlUuid,
    Field SqlTimestamptz,
    Field SqlTimestamptz,
    FieldNullable SqlTimestamptz,
    Field SqlText,
    Field SqlText,
    FieldNullable SqlText,
    FieldNullable SqlText,
    FieldNullable SqlText
  )

documents :: DocumentsTable
documents =
  table
    "documents"
    ( p10
        ( readOnlyTableField "id",
          requiredTableField "uuid",
          requiredTableField "created_date",
          requiredTableField "published_date",
          optionalTableField "updated_date",
          requiredTableField "canonical_url",
          requiredTableField "doc_type",
          optionalTableField "content",
          optionalTableField "colophon",
          optionalTableField "page_src_url"
        )
    )

type EntriesTable = Table EntriesW EntriesR

type EntriesW =
  ( (),
    Field SqlInt4,
    Field SqlInt4,
    Field SqlInt4,
    Field SqlInt4,
    Field SqlInt4,
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText),
    Maybe (Field (SqlArray SqlText)),
    Maybe (Field (SqlArray SqlText)),
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText)
  )

type EntriesR =
  ( Field SqlInt4,
    Field SqlInt4,
    Field SqlInt4,
    Field SqlInt4,
    Field SqlInt4,
    Field SqlInt4,
    FieldNullable SqlText,
    FieldNullable SqlText,
    FieldNullable SqlText,
    FieldNullable SqlText,
    Field (SqlArray SqlText),
    Field (SqlArray SqlText),
    FieldNullable SqlText,
    FieldNullable SqlText,
    FieldNullable SqlText
  )

entries :: EntriesTable
entries =
  table
    "entries"
    ( p15
        ( readOnlyTableField "id",
          requiredTableField "document",
          requiredTableField "year",
          requiredTableField "month",
          requiredTableField "day",
          requiredTableField "ordinal",
          optionalTableField "short_name",
          optionalTableField "summary",
          optionalTableField "location",
          optionalTableField "short_name",
          optionalTableField "photos",
          optionalTableField "reply_to",
          optionalTableField "repost_of",
          optionalTableField "rsvp",
          optionalTableField "rsvp_to"
        )
    )

type ProjectsTable =
  Table
    ProjectsW
    ProjectsR

type ProjectsW =
  ( (),
    Field SqlInt4,
    Field SqlText,
    Field SqlText,
    Field SqlInt2,
    Field SqlTimestamptz,
    Maybe (FieldNullable SqlTimestamptz),
    Field SqlTimestamptz,
    Field SqlText,
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText),
    Maybe (FieldNullable SqlText)
  )

type ProjectsR =
  ( Field SqlInt4,
    Field SqlInt4,
    Field SqlText,
    Field SqlText,
    Field SqlInt2,
    Field SqlTimestamptz,
    FieldNullable SqlTimestamptz,
    Field SqlTimestamptz,
    Field SqlText,
    FieldNullable SqlText,
    FieldNullable SqlText,
    FieldNullable SqlText,
    FieldNullable SqlText
  )

projects :: ProjectsTable
projects =
  table
    "projects"
    ( p13
        ( readOnlyTableField "id",
          requiredTableField "document",
          requiredTableField "slug",
          requiredTableField "status",
          requiredTableField "featured_order",
          requiredTableField "started_date",
          optionalTableField "finished_date",
          requiredTableField "sort_date",
          requiredTableField "name",
          optionalTableField "summary",
          optionalTableField "url",
          optionalTableField "source",
          optionalTableField "location"
        )
    )