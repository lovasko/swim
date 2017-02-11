{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Decompress.Perform
( DecompressOptions
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
data DecompressOptions = DecompressOptions { decompressFile :: FilePath }
  deriving (Show)

-- | Data file to compress.
optFile :: Parser FilePath -- ^ parser
optFile = argument str (metavar "FILE")

-- | Command-line user interface.
options :: Parser DecompressOptions -- ^ parser
options = DecompressOptions <$> optFile

-- | Load a story and save it decompressed.
decompress :: FilePath -- ^ file path
           -> IO ()    -- ^ action
decompress path = storyLoad path >>= \case
  Left  err   -> T.putStrLn ("ERROR: " <> err)           >> exitFailure
  Right story -> storySave story (fmtChange FmtRaw path) >> exitSuccess

-- | Peform story compression on a selected file.
perform :: DecompressOptions -- ^ decompress options
        -> IO ()             -- ^ command action
perform opts = case fmtIdentify path of
  Left err     -> T.putStrLn ("ERROR: " <> err)            >> exitFailure
  Right FmtRaw -> T.putStrLn "ERROR: Already decompressed" >> exitFailure
  Right FmtZip -> decompress path
  where path = decompressFile opts
