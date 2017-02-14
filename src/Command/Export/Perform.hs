{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Export.Perform
( ExportOptions
, options
, perform
) where

import Data.Monoid
import Options.Applicative
import System.Exit
import Text.Comma
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.FilePath as F

import Load
import Story
import Util


-- | Options of the "export-csv" command.
data ExportOptions = ExportOptions
  { exportNoHeader  :: Bool
  , exportTimestamp :: Bool
  , exportFile      :: FilePath }
  deriving (Show)

-- | Switch to trigger exclusion of a CSV header.
optionNoHeader :: Parser Bool -- ^ parser
optionNoHeader = switch
   $ long    "n"
  <> long    "no-header"
  <> help    "Header-less output table"

-- | Switch to trigger timestamps instead of formatted dates.
optionTimestamp :: Parser Bool -- ^ parser
optionTimestamp = switch
   $ short   't'
  <> long    "timestamp"
  <> help    "Display time values as timestamps"

-- | Data file to export into CSV.
optionFile :: Parser FilePath -- ^ parser
optionFile = argument str (metavar "FILE")

-- | Command-line user interface.
options :: Parser ExportOptions -- ^ parser
options = ExportOptions <$> optionNoHeader <*> optionTimestamp <*> optionFile

-- | Create a CSV-formatted table of a story.
table :: ExportOptions -- ^ command-line options
      -> Story         -- ^ story
      -> T.Text        -- ^ CSV table
table opts story = uncomma $ bool rows (headers:rows) (exportNoHeader opts)
  where
    headers = ["time", "value"]
    rows    = map conv story
    conv (time, val) = [showTime (exportTimestamp opts) time, textShow val]

-- | Pretty-print the content of a story file.
perform :: ExportOptions -- ^ options
        -> IO ()         -- ^ command action
perform opts = storyLoad file >>= \case
  Left  msg   -> errorPrint msg                         >> exitFailure
  Right story -> T.writeFile newName (table opts story) >> exitSuccess
  where
    file    = exportFile opts
    newName = F.replaceExtension file "csv"
