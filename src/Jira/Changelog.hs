{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Jira.Changelog ( ChangeDetails
                      , Changelog
                      , PageBeanChangelog
                      , changeDetailsContainsField
                      , filterChangeDetailsByField
                      , filterChangelogByField
                      , getChangeDetailsFromChangelog
                      , getChangeFromTo
                      , getChangelogFromBean
                      ) where

import GHC.Generics
import Data.List as L

import Data.Aeson
import Data.Text

import Jira.Aliases (Field, FromTo, Uri)
import Jira.UserDetails

{-
Jira issue changelog model:

https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issues/#api-rest-api-3-issue-issueidorkey-changelog-get
-}

data PageBeanChangelog =
  PageBeanChangelog
    { self :: !Uri
    , nextPage :: !(Maybe Uri)
    , maxResults :: Integer -- int32
    , startAt :: Integer -- int64
    , total :: Integer -- int64
    , isLast :: Bool
    , values :: [Changelog]
    }
  deriving (Show)

instance FromJSON PageBeanChangelog where
  parseJSON = withObject "PageBeanChangelog" $ \v -> PageBeanChangelog
    <$> v .:  "self"
    <*> v .:? "nextPage"
    <*> v .:  "maxResults"
    <*> v .:  "startAt"
    <*> v .:  "total"
    <*> v .:  "isLast"
    <*> v .:  "values"


data Changelog =
  Changelog
    { id :: !Text
    , author :: UserDetails
    , created :: !Text
    , items :: [ChangeDetails]
 -- , historyMetadata  -- ignore
    }
  deriving (Generic, Show)

instance FromJSON Changelog

data ChangeDetails =
  ChangeDetails
    { field :: !Field
    , fieldtype :: !Text
    , fieldId :: !(Maybe Text)
    , from :: !(Maybe Text)
    , fromString :: !(Maybe Text)
    , to :: !(Maybe Text)
    , toString :: !(Maybe Text)
    }
  deriving (Generic, Show)

instance FromJSON ChangeDetails where
  parseJSON = withObject "ChangeDetails" $ \v -> ChangeDetails
    <$> v .:  "field"
    <*> v .:  "fieldtype"
    <*> v .:? "fieldId"
    <*> v .:? "from"
    <*> v .:? "fromString"
    <*> v .:? "to"
    <*> v .:? "toString"


getChangelogFromBean :: PageBeanChangelog -> [Changelog]
getChangelogFromBean = values

filterChangelogByField :: Field -> [Changelog] -> [Changelog]
filterChangelogByField fld = L.filter (L.any (changeDetailsContainsField fld) . items)

getChangeDetailsFromChangelog :: Changelog -> [ChangeDetails]
getChangeDetailsFromChangelog = items

filterChangeDetailsByField :: Field -> [ChangeDetails] -> [ChangeDetails]
filterChangeDetailsByField = L.filter . changeDetailsContainsField

changeDetailsContainsField :: Field -> ChangeDetails -> Bool
changeDetailsContainsField = (. field) . (==)

getChangeFromTo :: Field -> [ChangeDetails] -> [FromTo]
getChangeFromTo f = L.map getFromTo . filterChangeDetailsByField f
  where getFromTo = (,) <$> fromString <*> toString
