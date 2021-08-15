{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Jira.Changelog ( Changelog(..)
                      , ChangeDetails(..)
                      , PageBeanChangelog(..)
                      ) where

import GHC.Generics

import Data.Aeson
import Data.Text

import Jira.Aliases (Uri)
import Jira.UserDetails


data PageBeanChangelog = PageBeanChangelog { self :: !Uri
                                           , nextPage :: !(Maybe Uri)
                                           , maxResults :: Integer  -- int32
                                           , startAt :: Integer -- int64
                                           , total :: Integer -- int64
                                           , isLast :: Bool
                                           , values :: [Changelog]
                                           } deriving Show

instance FromJSON PageBeanChangelog where
  parseJSON = withObject "PageBeanChangelog" $ \v -> PageBeanChangelog
    <$> v .:  "self"
    <*> v .:? "nextPage"
    <*> v .:  "maxResults"
    <*> v .:  "startAt"
    <*> v .:  "total"
    <*> v .:  "isLast"
    <*> v .:  "values"


data Changelog = Changelog { id :: !Text
                           , author :: UserDetails
                           , created :: !Text
                           , items :: [ChangeDetails]
                           -- ignore: , historyMetadata
                           } deriving (Generic, Show)

instance FromJSON Changelog


data ChangeDetails = ChangeDetails { field :: !Text
                                   , fieldtype :: !Text
                                   , fieldId :: !(Maybe Text)
                                   , from :: !(Maybe Text)
                                   , fromString :: !(Maybe Text)
                                   , to :: !(Maybe Text)
                                   , toString :: !(Maybe Text)
                                   } deriving (Generic, Show)

instance FromJSON ChangeDetails where
  parseJSON = withObject "ChangeDetails" $ \v -> ChangeDetails
    <$> v .:  "field"
    <*> v .:  "fieldtype"
    <*> v .:? "fieldId"
    <*> v .:? "from"
    <*> v .:? "fromString"
    <*> v .:? "to"
    <*> v .:? "toString"
