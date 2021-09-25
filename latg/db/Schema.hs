-- oh god why
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module LATG.DB.Schema where 

import Opaleye 

data EntryT a b c d e f g h i j k l m n o p q r s t = Entry
  { id :: a
  , uuid :: b
  , year :: c
  , month :: d
  , day :: e
  , ordinal :: f
  , slug :: g
  , createdDate :: h 
  , publishedDate :: i 
  , updatedDate :: j 
  , name :: k
  , summary :: l
  , location :: m
  , photos :: n
  , replyTo :: o
  , repostOf :: p
  , rsvp :: q
  , content :: r
  , colophon :: s
  , pageSrcUrl :: t
  }

data Project a b c d e f g h i j k l m n o = Project
  { id :: a
  , uuid :: b
  , slug :: c
  , status :: d
  , featured_order :: e
  , started_date :: f
  , finished_date :: g
  , published_date :: h
  , updated_date :: i
  , name :: j
  , summary :: k
  , url :: l
  , source :: m
  , location :: n
  , content :: o
  }