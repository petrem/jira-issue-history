{-# LANGUAGE OverloadedStrings #-}

module Output where

import Data.Maybe (mapMaybe)

import Data.Aeson
import qualified Data.HashMap.Lazy as HML
import Data.Text (Text)
import qualified Data.Text as T

import Jira.Aliases (IssueKey)
import qualified Jira.Changelog as CL


-- Each PBChangelog has multiple Changelogs (values) representing a change of potentially many fields
-- each described in a ChangeDetails.
-- We're interested in Changelog items modifying 'Key' fields

type FromTo = (Maybe Text, Maybe Text)

parsePageBeanChangelog :: CL.PageBeanChangelog -> [FromTo]
parsePageBeanChangelog = mapMaybe parseChangelog . CL.values

-- there should be at most one change on the "Key" field
parseChangelog :: CL.Changelog -> Maybe FromTo
parseChangelog changes =
  case length keyChanges of
    0 -> Nothing
    1 -> Just $ head keyChanges
    _ -> errorWithoutStackTrace "Error: Response contains more than one Key change per changelog"
  where keyChanges = mapMaybe parseChangeDetails . CL.items $ changes

parseChangeDetails :: CL.ChangeDetails -> Maybe FromTo
parseChangeDetails details = if CL.field details == "Key"
                             then Just (CL.fromString details, CL.toString details)
                             else Nothing

mergeObjects :: [Value] -> Value
mergeObjects = Object . HML.unions . map (\(Object x) -> x)

reprChanges :: [(IssueKey, [FromTo])] -> Text
reprChanges = T.unlines . map reprChange

reprChange :: (IssueKey, [FromTo]) -> Text
reprChange (key, cs) = T.concat [key, ": ", T.intercalate " -> " $  foldr go [] cs]
  where
        go (mfrom, mto) l =
          case (mfrom, mto) of (Nothing, Nothing) -> l
                               (Nothing, Just to) -> emptyKey : to .:. l
                               (Just from, Nothing) -> from : emptyKey .:. l
                               (Just from, Just to) -> from : to .:. l

emptyKey :: Text
emptyKey = "<empty>"

(.:.) :: Eq a => a -> [a] -> [a]
x .:. [] = [x]
x .:. orig@(y:_) | x == y    = orig
                 | otherwise = x:orig
