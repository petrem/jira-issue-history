{-# LANGUAGE OverloadedStrings #-}

module Jira.API ( IssueChangelogResult
                , getIssuesChangeLogs
                , getIssuesChangeLogsFromJQL
                , getJQL
                , mkBaseApiUrl
                , mkCloudUrlFromSite
                , mkJiraSite
                ) where

import Control.Monad (forM, (<=<))
import qualified Data.ByteString as B

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TSE

import Control.Lens
import qualified Network.HTTP.Client as NC
import Network.Wreq
import qualified Network.Wreq.Session as BreqS

import Jira.Aliases (IssueKey, JQLQuery)
import Jira.Changelog (Changelog, getChangelogFromBean)
import Jira.JQL (SearchResults, getIssueKeysFromSearchResults)

import Replace (replaceOne, mkReplacement)


data JiraSite = JiraSite { siteUrl :: T.Text
                         , siteUser :: B.ByteString
                         , siteToken :: B.ByteString
                         }

-- Configure Jira API
cloudUrlTemplate :: Text
cloudUrlTemplate = "https://{site}.atlassian.net"

mkCloudUrlFromSite :: Text -> Text
mkCloudUrlFromSite site = replaceOne (mkReplacement "{site}" site) cloudUrlTemplate

mkBaseApiUrl :: Text -> Text
mkBaseApiUrl serverUrl =
  T.append (T.dropWhileEnd (== '/') . T.strip $ serverUrl) "/rest/api/3"

mkJiraSite :: Text -> Text -> Text -> JiraSite
mkJiraSite url user token = JiraSite url (TSE.encodeUtf8 user) (TSE.encodeUtf8 token)

responseChecker :: NC.Request -> Response NC.BodyReader -> IO ()
responseChecker _ _ = do
  return ()


-- Issues
type IssueChangelogResult = (IssueKey, Either Text [Changelog])


getIssuesChangeLogs ::
     JiraSite -> [IssueKey] -> IO [IssueChangelogResult]
getIssuesChangeLogs api issueKeys = do
  let opts = defaults &
        (header "Accept" .~ ["application/json"])
        . (auth ?~ basicAuth (siteUser api) (siteToken api))
        . (checkResponse ?~ responseChecker)
  BreqS.withSession $ \sess -> do
    forM issueKeys (getIssueChangeLog api sess opts)


getIssueChangeLog ::
     JiraSite
  -> BreqS.Session
  -> Options
  -> IssueKey
  -> IO IssueChangelogResult
getIssueChangeLog api sess opts issueKey = do
  let url = T.concat [siteUrl api, "/issue/", issueKey, "/changelog"]
  r <- BreqS.getWith opts sess (T.unpack url)
  let httpCode = r ^. responseStatus . statusCode
  result <- case httpCode of
        200 -> do
          jr <- asJSON r
          return $ Right (getChangelogFromBean $ jr ^. responseBody)
        401 -> return $ Left "401 Unauthorized"
        404 -> return $ Left "404 Not found"
        _ -> return $ Left (T.concat ["Unexpected error: ", T.pack $ show httpCode])
  return (issueKey, result)

-- Search using JQL

getJQL ::
  JiraSite
  -> (Options -> Options)
  -> JQLQuery
  -> IO (Either Text SearchResults)
getJQL api extraOpts jql = do
  let opts = defaults &
        (header "Accept" .~ ["application/json"])
        . (auth ?~ basicAuth (siteUser api) (siteToken api))
        . (checkResponse ?~ responseChecker)
  let opts' = opts & param "jql" .~ [jql] & extraOpts
  let url = T.concat [siteUrl api, "/search"]
  r <- getWith opts' (T.unpack url)
  let httpCode = r ^. responseStatus . statusCode
  case httpCode of
    200 -> do
      jr <- asJSON r
      return $ Right $ jr ^. responseBody
    400 -> return $ Left "400 JQL query is invalid"
    401 -> return $ Left "401 Unauthorized"
    _ -> return $ Left (T.concat ["Unexpected error: ", T.pack $ show httpCode])

-- data SearchParams =
--   SearchParams
--     { jqlExpand :: Maybe Text
--     , jqlStartAt :: Maybe Integer
--     , jqlMaxResults :: Maybe Integer
--     , validateQuery :: Maybe Text
--     }
--   deriving Show

-- searchDefaultParams = SearchParams Nothing Nothing Nothing Nothing

-- addSearchParams :: SearchParams -> Options -> Options
-- addSearchParams sp origOpt = origOpt
--   & maybeSetParam "expand" (jqlExpand sp)

-- maybeSetParam :: Text -> Maybe Text -> Options -> Options
-- maybeSetParam _ Nothing = id
-- maybeSetParam k (Just v) = param k .~ [v]

-- setParam :: Text -> Text -> Options -> Options
-- setParam k v = param k .~ [v]

--

getIssuesChangeLogsFromJQL :: JiraSite -> JQLQuery -> IO (Either Text [IssueChangelogResult])
getIssuesChangeLogsFromJQL api =
  traverse (getIssuesChangeLogs api . getIssueKeysFromSearchResults) <=< getJQL api id
