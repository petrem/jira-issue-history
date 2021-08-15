{-# LANGUAGE OverloadedStrings #-}

module ExpandUserPath
    (expandUserPath
    ) where

import Control.Applicative (liftA2)
import Data.List (isPrefixOf)

import System.Directory (getHomeDirectory)
import System.FilePath (combine, normalise, joinPath, splitDirectories)
import System.Posix.User (UserEntry (homeDirectory), getUserEntryForName)


expandUserPath :: FilePath -> IO FilePath
expandUserPath path = let npath = normalise path
                      in if "~" `isPrefixOf` npath
                         then userExpandedPath npath
                         else return npath
  where
    userExpandedPath p = let (tildePart:rest) = splitDirectories p
                         in liftA2 combine (getUserHome tildePart) (pure $ joinPath rest)
    getUserHome ('~':user) = case user of
                               "" -> getHomeDirectory
                               _  -> homeDirectory <$> getUserEntryForName user
    getUserHome p = error $ "Shouldn't happen: " ++ p ++ " is supposed to start with ~"

