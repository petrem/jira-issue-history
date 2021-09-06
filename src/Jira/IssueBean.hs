{-# LANGUAGE DeriveGeneric #-}

module Jira.IssueBean ( IssueBean
                      , getChangelogFromIssueBean
                      , getIssueKeyFromIssueBean
                      ) where

import GHC.Generics

import Data.Aeson
import Data.Text

import Jira.Aliases (IssueKey, Uri)
import Jira.Changelog (Changelog)

{-
Jira issue IssueBean

see https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-search/#api-rest-api-3-search-post
-}

data IssueBean =
  IssueBean
    { expand :: Text
    , id :: Text
    , self :: Uri
    , key :: IssueKey
--  , renderedFields :: Object
--  , properties :: Object
--  , names :: Object
--  , schema :: Object
--  , transitions :: [IssueTransition]
--  , operations :: Operations
--  , editmeta :: IssueUpdateMetadata
    , changelog :: Maybe PageOfChangelogs
--  , versionedRepresentations :: Object
--  , fieldsToInclude :: IncludedFields
--  , fields :: Object
    }
  deriving (Show, Generic)

instance FromJSON IssueBean


data PageOfChangelogs =
  PageOfChangelogs
    { startAt :: Integer -- int32
    , maxResults :: Integer -- int32
    , total :: Integer -- int32
    , histories :: [Changelog]
    }
  deriving (Show, Generic)

instance FromJSON PageOfChangelogs

getChangelogFromIssueBean :: IssueBean -> (IssueKey, Maybe [Changelog])
getChangelogFromIssueBean = (,) <$> key <*> ((histories <$>) . changelog)

getIssueKeyFromIssueBean :: IssueBean -> IssueKey
getIssueKeyFromIssueBean = key
