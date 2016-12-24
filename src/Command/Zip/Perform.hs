{-# LANGUAGE OverloadedStrings #-}

module Command.Zip.Perform
( perform
) where

import Data.Monoid
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Command.Zip.Options
import Load
import Save
import Variant


-- | Load a story and save it right back compressed.
compress :: FilePath -- ^ file path
         -> IO ()    -- ^ action
compress path = do
  result <- storyLoad path
  case result of
    Left  err   -> T.putStrLn ("ERROR: " <> err) >> exitFailure
    Right story -> storySave story path          >> exitSuccess

-- | Peform story compression on a selected file.
perform :: ZipOptions -- ^ compress options
        -> IO ()      -- ^ command action
perform options = case varIdentify path of
  Left err     -> T.putStrLn ("ERROR: " <> err)          >> exitFailure
  Right VarZip -> T.putStrLn "ERROR: Already compressed" >> exitFailure
  Right VarRaw -> compress path
  where path = zipOptFile options
