{-# LANGUAGE OverloadedStrings #-}

module Jih.ConfigFiles (Config(..)
                       , defaultConfigFile
                       , defaultTokenFile
                       , mkConfig
                       , parseConfig
                       , readTokenFile
                       ) where

import qualified Data.ByteString.Lazy as BS.L

import Data.Text (Text)
import qualified Data.Text as T
import Data.YAML


parseConfig :: FilePath -> IO (Either (Pos, String) Config)
parseConfig = (decode1 <$>) . BS.L.readFile

data Config = Config
    { configServerUrl :: Text
    , configUserName  :: Text
    , configAPIToken  :: Text
    } deriving Show


instance FromYAML Config where
   parseYAML = withMap "Config" $ \m -> Config
       <$> m .: "atlassian_server"
       <*> m .: "atlassian_username"
       <*> m .: "atlassian_api_token"

mkConfig :: Text -> Text -> Text -> Config
mkConfig = Config

-- token file

defaultTokenFile :: FilePath
defaultTokenFile = "~/.jira"

readTokenFile :: FilePath -> IO Text
readTokenFile = fmap (T.strip . T.pack) . readFile

-- atlassian site settings file
-- we're looking for the following keys:
-- atlassian_server (URL), atlassian_username, atlassian_token (API token)

defaultConfigFile :: FilePath
defaultConfigFile = "~/.atlassian.yaml"

