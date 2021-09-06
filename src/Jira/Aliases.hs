module Jira.Aliases ( AccountType
                    , Field
                    , FromTo
                    , IssueKey
                    , JQLQuery
                    , Uri
                    ) where

import Data.Text


-- account type is one of: 'atlassian' (normal user), 'app' (application user) or
-- 'customer' (Jira Service Desk customer user)
type AccountType = Text

type FromTo = (Maybe Text, Maybe Text)

type Field = Text

type JQLQuery = Text

type IssueKey = Text

type Uri = Text
