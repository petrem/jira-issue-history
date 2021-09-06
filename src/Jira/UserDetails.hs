{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Jira.UserDetails (UserDetails) where

import GHC.Generics

import Data.Aeson
import Data.Text (Text)

import Jira.Aliases (AccountType, Uri)


data UserDetails =
  UserDetails
    { self :: !Uri
 -- , name :: Text -- deprecated, ignoring
 -- , key :: Text -- deprecated, ignoring
    , accountId :: !Text -- max 128
    , emailAddress :: !Text -- may be null
    , avatarUrls :: AvatarUrlsBean
    , displayName :: !Text
    , active :: Bool
    , timeZone :: !Text -- may be null
    , accountType :: AccountType
    }
  deriving (Generic, Show)

instance FromJSON UserDetails

data AvatarUrlsBean = AvatarUrlsBean { _16x16 :: !(Maybe Uri)
                                     , _24x24 :: !(Maybe Uri)
                                     , _32x32 :: !(Maybe Uri)
                                     , _64x64 :: !(Maybe Uri)
                                     } deriving Show

instance FromJSON AvatarUrlsBean where
    parseJSON = withObject "AvatarUrlsBean" $ \v -> AvatarUrlsBean
      <$> v .:? "16x16"
      <*> v .:? "24x24"
      <*> v .:? "32x32"
      <*> v .:? "64x64"
