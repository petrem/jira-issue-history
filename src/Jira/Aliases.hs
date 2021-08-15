module Jira.Aliases ( AccountType
                    , IssueKey
                    , Uri
                    ) where

import Data.Text


-- account type is one of: 'atlassian' (normal user), 'app' (application user) or
-- 'customer' (Jira Service Desk customer user)
type AccountType = Text

type IssueKey = Text

type Uri = Text
