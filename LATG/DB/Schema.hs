-- oh god why
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module LATG.DB.Schema where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
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

data Document a b c d e f g h i j = Document
  { docId :: a,
    uuid :: b,
    createdDate :: c,
    publishedDate :: d,
    updatedDate :: e,
    canonicalUrl :: f,
    docType :: g,
    content :: h,
    colophon :: i,
    pageSrcUrl :: j
  }

$(makeAdaptorAndInstance "pDocument" ''Document)

type DocumentW =
  Document
    ()
    (Field SqlUuid)
    (Field SqlTimestamptz)
    (Field SqlTimestamptz)
    (FieldNullable SqlTimestamptz)
    (Field SqlText)
    (Field SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)

type DocumentR =
  Document
    (Field SqlInt4)
    (Field SqlUuid)
    (Field SqlTimestamptz)
    (Field SqlTimestamptz)
    (FieldNullable SqlTimestamptz)
    (Field SqlText)
    (Field SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)

type DocumentT = Table DocumentW DocumentR

documents :: DocumentT
documents =
  table
    "documents"
    ( pDocument $
        Document
          { docId = readOnlyTableField "id",
            uuid = requiredTableField "uuid",
            createdDate = requiredTableField "created_date",
            publishedDate = requiredTableField "published_date",
            updatedDate = requiredTableField "updated_date",
            canonicalUrl = requiredTableField "canonical_url",
            docType = requiredTableField "doc_type",
            content = requiredTableField "content",
            colophon = requiredTableField "colophon",
            pageSrcUrl = requiredTableField "page_src_url"
          }
    )

data Entry a b c d e f g h i j k l m n o = Entry
  { entryId :: a,
    entryDocument :: b,
    year :: c,
    month :: d,
    day :: e,
    ordinal :: f,
    shortName :: g,
    entryName :: h,
    entrySummary :: i,
    entryLocation :: j,
    photos :: k,
    replyTo :: l,
    repostOf :: m,
    rsvpTo :: n,
    rsvpValue :: o
  }

$(makeAdaptorAndInstance "pEntry" ''Entry)

type EntryT = Table EntryW EntryR

type EntryW =
  Entry
    ()
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlInt4)
    (Maybe (FieldNullable SqlText))
    (Maybe (FieldNullable SqlText))
    (Maybe (FieldNullable SqlText))
    (Maybe (FieldNullable SqlText))
    (Maybe (Field (SqlArray SqlText)))
    (Maybe (Field (SqlArray SqlText)))
    (Maybe (FieldNullable SqlText))
    (Maybe (FieldNullable SqlText))
    (Maybe (FieldNullable SqlText))

type EntryR =
  Entry
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlInt4)
    (FieldNullable SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)
    (Field (SqlArray SqlText))
    (Field (SqlArray SqlText))
    (FieldNullable SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)

entries :: EntryT
entries =
  table
    "entries"
    ( pEntry $
        Entry
          { entryId = readOnlyTableField "id",
            entryDocument = requiredTableField "document",
            year = requiredTableField "year",
            month = requiredTableField "month",
            day = requiredTableField "day",
            ordinal = requiredTableField "ordinal",
            shortName = optionalTableField "short_name",
            entryName = optionalTableField "summary",
            entrySummary = optionalTableField "location",
            entryLocation = optionalTableField "short_name",
            photos = optionalTableField "photos",
            replyTo = optionalTableField "reply_to",
            repostOf = optionalTableField "repost_of",
            rsvpTo = optionalTableField "rsvp",
            rsvpValue = optionalTableField "rsvp_to"
          }
    )

data Project a b c d e f g h i j k l m = Project
  { projectId :: a,
    projectDocument :: b,
    slug :: c,
    status :: d,
    featured_order :: e,
    started_date :: f,
    finished_date :: g,
    sort_date :: h,
    projectName :: i,
    projectSummary :: j,
    url :: k,
    source :: l,
    projectLocation :: m
  }

$(makeAdaptorAndInstance "pProject" ''Project)

type ProjectT = Table ProjectW ProjectR

type ProjectW =
  Project
    ()
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlText)
    (Field SqlInt2)
    (Field SqlTimestamptz)
    (Maybe (FieldNullable SqlTimestamptz))
    (Field SqlTimestamptz)
    (Field SqlText)
    (Maybe (FieldNullable SqlText))
    (Maybe (FieldNullable SqlText))
    (Maybe (FieldNullable SqlText))
    (Maybe (FieldNullable SqlText))

type ProjectR =
  Project
    (Field SqlInt4)
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlText)
    (Field SqlInt2)
    (Field SqlTimestamptz)
    (FieldNullable SqlTimestamptz)
    (Field SqlTimestamptz)
    (Field SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)
    (FieldNullable SqlText)

projects :: ProjectT
projects =
  table
    "projects"
    ( pProject $
        Project
          { projectId = readOnlyTableField "id",
            projectDocument = requiredTableField "document",
            slug = requiredTableField "slug",
            status = requiredTableField "status",
            featured_order = requiredTableField "featured_order",
            started_date = requiredTableField "started_date",
            finished_date = optionalTableField "finished_date",
            sort_date = requiredTableField "sort_date",
            projectName = requiredTableField "name",
            projectSummary = optionalTableField "summary",
            url = optionalTableField "url",
            source = optionalTableField "source",
            projectLocation = optionalTableField "location"
          }
    )
