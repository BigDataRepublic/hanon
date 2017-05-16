{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Control.Applicative
import Control.Monad (when)
import Options
import Database.LevelDB.Higher (runCreateLevelDB, KeySpace)

data MainOptions = MainOptions
    { optScan :: Bool
    , optList :: Bool
    , optMap :: Bool
    , optDirect :: Bool
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "scan" False
            "Scan the given files to create a anonimizing translation mappings"
        <*> simpleOption "list" False
            "List all mappings"
        <*> simpleOption "map" False
            "Apply all possible mappings to the given files and output .anon versions"
        <*> simpleOption "direct" False
            "Apply all possible mappings to the given files and output .anon versions, without using LevelDB for storage"

main :: IO ()
main = runCommand $ \opts args -> do
    runCreateLevelDB "hanon_mapping" "hanon" $ do
        when (optScan opts) $ scanFiles args
        when (optList opts) listMapping
        when (optMap opts) $ mapFiles args
    when (optDirect opts) $ mapDirectFiles args
