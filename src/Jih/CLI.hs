module Jih.CLI (jihCLI) where

import Data.Version (showVersion)

import Data.Text (Text)

import Options.Applicative

import Jira.Aliases (IssueKey, JQLQuery)
import Jira.API ( getIssuesChangeLogs
                , getIssuesChangeLogsFromJQL
                , mkBaseApiUrl
                , mkCloudUrlFromSite
                , mkJiraSite
                )

import Jih.ExpandUserPath (expandUserPath)
import Jih.ConfigFiles (Config(..)
                       , defaultConfigFile
                       , defaultTokenFile
                       , mkConfig
                       , parseConfig
                       , readTokenFile
                       )
import Jih.Output.Console (displayErrors, displayKeyChanges)

import Paths_jira_issue_history (version)


jihCLI :: IO ()
jihCLI = do
  options <- execParser opts
  config <-
    case configOpts options of
      (CliConfigOpts tokenFile siteName userAccount) -> do
        token <- readTokenFile =<< expandUserPath tokenFile
        return $ mkConfig (mkCloudUrlFromSite siteName) userAccount token
      (ConfigFileOpt configFile) -> do
        parseResult <- parseConfig =<< expandUserPath configFile
        case parseResult of
          (Right config) -> return config
          (Left (pos, err)) ->
            fail $ "Failed to parse config at " ++ show pos ++ ": " ++ err
  let api =
        mkJiraSite
          <$> (mkBaseApiUrl . configServerUrl)
          <*> configUserName
          <*> configAPIToken
          $ config
  results <-
    case queryOpts options of
      IssuesFlag -> do
        Right <$> getIssuesChangeLogs api (getIssueKeys . issueArgs $ options)
      (JQLOpt jql) -> do
        getIssuesChangeLogsFromJQL api jql

  displayKeyChanges results
  displayErrors results


-- CLI Parsing

data CLIOpts = CLIOpts
    { configOpts :: !ConfigOpts
    , queryOpts :: QueryOpts
    , issueArgs :: IssueArgs
    }
    deriving Show

data ConfigOpts
  = CliConfigOpts
      { getTokenFile :: FilePath
      , getSiteName :: Text
      , getUserAccount :: Text
      }
  | ConfigFileOpt
      { getConfigFile :: FilePath
      }
  deriving (Show)

data QueryOpts
  = IssuesFlag
  | JQLOpt
      { jqlQuery :: JQLQuery
      }
  deriving (Show)

newtype IssueArgs =
  IssueArgs
    { getIssueKeys :: [IssueKey]
    }
  deriving (Show)


opts :: ParserInfo CLIOpts
opts = info (cliParser <**> versionOption <**> helper)
  ( fullDesc
    <> progDesc "Fetch Jira issues changelogs and print Key changes history"
    <> header "jira-issue-history - print Jira issues key history"
  )

cliParser :: Parser CLIOpts
cliParser = CLIOpts
            <$> (configFileParser <|> cliConfigOptsParser)
            <*> (jqlOptParser <|> issuesFlagParser)
            <*> issueKeysParser


versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version) (long "version" <> help "Show version.")

cliConfigOptsParser :: Parser ConfigOpts
cliConfigOptsParser =
  CliConfigOpts <$> strOption
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
  )

configFileParser :: Parser ConfigOpts
configFileParser =
  ConfigFileOpt   <$> strOption
  ( long "config-file"
    <> short 'c'
    <> metavar "CONFIG-FILE"
    <> help "YAML config file."
    <> showDefault
    <> value defaultConfigFile
  )

issuesFlagParser :: Parser QueryOpts
issuesFlagParser =
  flag' IssuesFlag
  ( long "issues"
    <> short 'i'
    <> help "Show changes for individual Jira issues given as arguments."
  )

issueKeysParser :: Parser IssueArgs
issueKeysParser = IssueArgs <$> many (argument str (metavar "ISSUE KEYS..."))

jqlOptParser :: Parser QueryOpts
jqlOptParser =
  JQLOpt <$> strOption
  ( long "jql"
    <> short 'j'
    <> metavar "JQL Query"
    <> help "Show changes for issues returned by JQL query."
  )
