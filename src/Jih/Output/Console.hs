{-# LANGUAGE OverloadedStrings #-}

module Jih.Output.Console ( displayErrors
                          , displayKeyChanges
                          ) where

import qualified Data.HashMap.Lazy as HML
import Data.List (sortBy)
import System.IO (stderr)

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Jira.Aliases (FromTo, IssueKey)
import Jira.API (IssueChangelogResult)
import qualified Jira.Changelog as CL


displayErrors :: Either Text [IssueChangelogResult] -> IO ()
displayErrors (Left err) = T.IO.hPutStrLn stderr err
displayErrors (Right icrs)
  | null errors = return ()
  | otherwise    = 
      T.IO.hPutStr stderr $
      T.concat ["\nErrors:\n\n"
               , T.concat
                 [T.concat [T.pack . show $ k, ": ", e, "\n"] | (k, e) <- sortTupleByFirst errors]
               ]
     where errors = [(k, e) | (k, Left e) <- icrs]

displayKeyChanges :: Either Text [IssueChangelogResult] -> IO ()
displayKeyChanges (Left _) = return ()
displayKeyChanges (Right icrs) =
    T.IO.putStr $
     T.concat ["Key changes:\n\n"
               , reprChanges $ (fmap . fmap) getKeyChanges (sortTupleByFirst changes)
              ]
    where changes = [(k, cs) | (k, Right cs) <- icrs]

getKeyChanges :: [CL.Changelog] -> [FromTo]
getKeyChanges =
  CL.getChangeFromTo "Key" .
  concatMap CL.getChangeDetailsFromChangelog

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

sortTupleByFirst :: Ord a => [(a, b)] -> [(a, b)]
sortTupleByFirst = sortBy (\(a, _) (b, _) -> compare a b)
