{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Either (lefts, rights)

import Data.Text (Text)
import qualified Data.Text as T

import Options.Applicative

import ExpandUserPath (expandUserPath)
import JiraAPI (mkJiraSite, getIssuesChangeLogs)
import Replace (mkReplacement, replaceOne)
import Output (parsePageBeanChangelog, reprChanges)


main :: IO ()
main = displayJiraIssueHistory =<< execParser opts
  where
    opts = info (cli <**> helper)
      ( fullDesc
     <> progDesc "Fetch Jira issues changelogs and print Key changes history"
     <> header "jira-issue-history - print Jira issues key history" )

data CLI = CLI
  { tokenFile  :: FilePath
  , siteName   :: Text
  , user       :: Text
  , issueKeys  :: [Text]
  }

cli :: Parser CLI
cli =
  CLI <$> strOption
  ( long "token-file"
    <> short 'f'
    <> metavar "TOKEN-FILE"
    <> help "File containing JIRA API token."
    <> showDefault
    <> value defaultTokenFile
  )
  <*> strOption
  ( long "site"
    <> short 's'
    <> help "Site name, as in https://<site name>.atlassian.net."
  ) <*> strOption
  ( long "user"
    <> short 'u'
    <> help "Your Atlassian username (email)."
  ) <*> some
  ( argument str
    ( metavar "ISSUE KEYS..." )
  )

displayJiraIssueHistory :: CLI -> IO ()
displayJiraIssueHistory (CLI t s u ks) = do
  file <- expandUserPath t
  token <- readTokenFile file
  let api = mkJiraSite (mkBaseJiraApiUrl s) u token
  cs <- getIssuesChangeLogs api ks
  let keyChangeChain = map (fmap parsePageBeanChangelog) $ rights cs
  let errors = lefts cs
  putStrLn . T.unpack . reprChanges $ keyChangeChain
  if null errors
    then return ()
    else putStrLn . T.unpack . T.concat $ ["\nErrors:\n\n", T.unlines errors]


-- JIRA API Access

baseJiraApiUrl :: Text
baseJiraApiUrl = "https://{site}.atlassian.net/rest/api/3"

mkBaseJiraApiUrl :: Text -> Text
mkBaseJiraApiUrl site = replaceOne (mkReplacement "{site}" site) baseJiraApiUrl


-- token file

defaultTokenFile :: FilePath
defaultTokenFile = "~/.jira"

readTokenFile :: FilePath -> IO Text
readTokenFile = fmap (T.strip . T.pack) . readFile

