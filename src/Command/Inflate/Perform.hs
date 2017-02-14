{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Inflate.Perform
( InflateOptions
, perform
, options
) where

import Options.Applicative
import System.Exit

import Format
import Load
import Save
import Util


-- | Command-line arguments.
data InflateOptions = InflateOptions { inflateFile :: FilePath }
  deriving (Show)

-- | Data file to inflate.
optFile :: Parser FilePath -- ^ parser
optFile = argument str (metavar "FILE")

-- | Command-line user interface.
options :: Parser InflateOptions -- ^ parser
options = InflateOptions <$> optFile

-- | Load a story and save it inflated.
inflate :: FilePath -- ^ file path
        -> IO ()    -- ^ action
inflate path = storyLoad path >>= \case
  Left  err   -> errorPrint err                          >> exitFailure
  Right story -> storySave story (fmtChange FmtRaw path) >> exitSuccess

-- | Perform story inflation on a selected file.
perform :: InflateOptions -- ^ inflate options
        -> IO ()          -- ^ command action
perform opts = case fmtIdentify path of
  Left  err    -> errorPrint err                >> exitFailure
  Right FmtRaw -> errorPrint "Already inflated" >> exitFailure
  Right FmtZip -> inflate path
  where path = inflateFile opts
