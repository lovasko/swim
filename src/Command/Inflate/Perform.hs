{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Inflate.Perform
( InflateOptions
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
  Left  err   -> T.putStrLn ("ERROR: " <> err)           >> exitFailure
  Right story -> storySave story (fmtChange FmtRaw path) >> exitSuccess

-- | Perform story inflation on a selected file.
perform :: InflateOptions -- ^ inflate options
        -> IO ()          -- ^ command action
perform opts = case fmtIdentify path of
  Left err     -> T.putStrLn ("ERROR: " <> err)        >> exitFailure
  Right FmtRaw -> T.putStrLn "ERROR: Already inflated" >> exitFailure
  Right FmtZip -> inflate path
  where path = inflateFile opts
