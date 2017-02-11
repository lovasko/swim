{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Compress.Perform
( CompressOptions
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
data CompressOptions = CompressOptions { compressFile :: FilePath }
  deriving (Show)

-- | Data file to compress.
optFile :: Parser FilePath -- ^ parser
optFile = argument str (metavar "FILE")

-- | Command-line user interface.
options :: Parser CompressOptions -- ^ parser
options = CompressOptions <$> optFile

-- | Load a story and save it right back compressed.
compress :: FilePath -- ^ file path
         -> IO ()    -- ^ action
compress path = storyLoad path >>= \case
  Left  err   -> T.putStrLn ("ERROR: " <> err)           >> exitFailure
  Right story -> storySave story (fmtChange FmtZip path) >> exitSuccess

-- | Peform story compression on a selected file.
perform :: CompressOptions -- ^ compress options
        -> IO ()           -- ^ command action
perform opts = case fmtIdentify path of
  Left err     -> T.putStrLn ("ERROR: " <> err)          >> exitFailure
  Right FmtZip -> T.putStrLn "ERROR: Already compressed" >> exitFailure
  Right FmtRaw -> compress path
  where path = compressFile opts
