{-# LANGUAGE DeriveGeneric #-}

module Jira.JQL ( SearchResults
                , getChangelogFromSearchResults
                , getIssueKeysFromSearchResults
                ) where

import GHC.Generics

import Data.Aeson
import Data.Text

import Jira.Aliases (IssueKey)
import Jira.Changelog (Changelog)
import Jira.IssueBean (IssueBean, getChangelogFromIssueBean, getIssueKeyFromIssueBean)


{-
Jira JQL search results:

https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-search/#
-}

data SearchResults =
  SearchResults
    { expand :: Maybe Text
    , startAt :: Integer -- int32
    , maxResults :: Integer -- int32
    , total :: Integer -- int32
    , issues :: [IssueBean]
    , warningMessages :: Maybe [Text]
  --, names :: Object
  --, schema :: Object
    }
  deriving (Show, Generic)

instance FromJSON SearchResults

getChangelogFromSearchResults :: SearchResults -> [(IssueKey, Maybe [Changelog])]
getChangelogFromSearchResults = fmap getChangelogFromIssueBean . issues

getIssueKeysFromSearchResults ::SearchResults -> [IssueKey]
getIssueKeysFromSearchResults = fmap getIssueKeyFromIssueBean . issues
