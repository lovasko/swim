{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Deflate.Perform
( DeflateOptions
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
data DeflateOptions = DeflateOptions { deflateFile :: FilePath }
  deriving (Show)

-- | Data file to deflate.
optFile :: Parser FilePath -- ^ parser
optFile = argument str (metavar "FILE")

-- | Command-line user interface.
options :: Parser DeflateOptions -- ^ parser
options = DeflateOptions <$> optFile

-- | Load a story and save it deflated.
deflate :: FilePath -- ^ file path
         -> IO ()    -- ^ action
deflate path = storyLoad path >>= \case
  Left  err   -> errorPrint err                          >> exitFailure
  Right story -> storySave story (fmtChange FmtZip path) >> exitSuccess

-- | Perform story deflation on a selected file.
perform :: DeflateOptions -- ^ deflate options
        -> IO ()          -- ^ command action
perform opts = case fmtIdentify path of
  Left  err    -> errorPrint err                >> exitFailure
  Right FmtZip -> errorPrint "Already deflated" >> exitFailure
  Right FmtRaw -> deflate path
  where path = deflateFile opts
