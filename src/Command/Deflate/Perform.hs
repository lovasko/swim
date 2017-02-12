{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Deflate.Perform
( DeflateOptions
, perform
, options
) where

import Data.Monoid
import Options.Applicative
import System.Exit
import qualified Data.Text.IO as T

import Load
import Save
import Format


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
  Left  err   -> T.putStrLn ("ERROR: " <> err)           >> exitFailure
  Right story -> storySave story (fmtChange FmtZip path) >> exitSuccess

-- | Perform story deflation on a selected file.
perform :: DeflateOptions -- ^ deflate options
        -> IO ()          -- ^ command action
perform opts = case fmtIdentify path of
  Left err     -> T.putStrLn ("ERROR: " <> err)        >> exitFailure
  Right FmtZip -> T.putStrLn "ERROR: Already deflated" >> exitFailure
  Right FmtRaw -> deflate path
  where path = deflateFile opts
