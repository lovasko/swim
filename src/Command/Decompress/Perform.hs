{-# LANGUAGE OverloadedStrings #-}

module Command.Unzip.Perform
( perform
) where

import Data.Monoid
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Command.Unzip.Options
import Load
import Save
import Variant


-- | Load a story and save it right back compressed.
compress :: FilePath -- ^ file path
         -> IO ()    -- ^ action
compress path = do
  result <- storyLoad path
  case story of
    Left  err   -> T.putStrLn ("ERROR: " <> err) >> exitFailure
    Right story -> storySave story path          >> exitSuccess

-- | Peform story compression on a selected file.
perform :: CompressOptions -- ^ compress options
        -> IO ()           -- ^ command action
perform options = case varIdentify path of
  Left err            = T.putStrLn ("ERROR: " <> err)            >> exitFailure
  Right VarNormal     = T.putStrLn "ERROR: Already decompressed" >> exitFailure
  Right VarCompressed = decompress path
  where path = unzipOptFile options
