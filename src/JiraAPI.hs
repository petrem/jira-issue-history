{-# LANGUAGE OverloadedStrings #-}

module JiraAPI ( mkJiraSite
               , getIssuesChangeLogs
               ) where

import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TSE

import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens
import Network.Wreq
import qualified Network.HTTP.Client as NC
import qualified Network.Wreq.Session as BreqS

import Jira.Aliases (IssueKey)
import Jira.Changelog (PageBeanChangelog)


data JiraSite = JiraSite { siteUrl :: T.Text
                         , siteUser :: B.ByteString
                         , siteToken :: B.ByteString
                         }

mkJiraSite :: Text -> Text -> Text -> JiraSite
mkJiraSite url user token = JiraSite url (TSE.encodeUtf8 user) (TSE.encodeUtf8 token)

getIssuesChangeLogs ::
     JiraSite -> [IssueKey] -> IO [Either Text (IssueKey, PageBeanChangelog)]
getIssuesChangeLogs api issueKeys = do
  let opts = defaults &
        (header "Accept" .~ ["application/json"])
        . (auth ?~ basicAuth (siteUser api) (siteToken api))
        . (checkResponse ?~ responseChecker)
  BreqS.withSession $ \sess -> do
    forM issueKeys (getIssueChangeLog api sess opts)
  where
    responseChecker :: NC.Request -> Response NC.BodyReader -> IO ()
    responseChecker _ _ = do
      return ()


getIssueChangeLog ::
     JiraSite
  -> BreqS.Session
  -> Options
  -> IssueKey
  -> IO (Either Text (IssueKey, PageBeanChangelog))
getIssueChangeLog api sess opts issueKey = do
  let url = T.concat [siteUrl api, "/issue/", issueKey, "/changelog"]
  r <- BreqS.getWith opts sess (T.unpack url)
  let httpCode = r ^. responseStatus . statusCode
  case httpCode of 200 -> do
                     jr <- asJSON r
                     return $ Right (issueKey, jr ^. responseBody)
                   404 -> return $ Left (T.concat ["Not found: ", issueKey])
                   _ -> return $ Left (T.concat ["Unexpected error: ", T.pack $show httpCode])

