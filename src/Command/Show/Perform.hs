{-# LANGUAGE OverloadedStrings #-}

module Command.Show.Perform
( perform
) where

import Data.Monoid
import Data.Word
import System.Exit
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Command.Show.Options
import Load
import Story
import Util


-- | Create a table entry from a story data point.
createEntry :: Bool            -- ^ pretty-print time
            -> (Word32, Float) -- ^ data point
            -> [T.Text]        -- ^ table entry
createEntry form (time, value) = [showTime form time, T.pack (show value)]

-- | Select a subset of the story based on the count command-line argument.
-- If no count was selected, the whole story is returned.
selectEntries :: [a]       -- ^ entries
              -> Maybe Int -- ^ count
              -> [a]       -- ^ entries subset
selectEntries xs Nothing   = xs
selectEntries xs (Just count)
  | abs count >= length xs = xs
  | count >= 0             = take count xs
  | otherwise              = drop (length xs + count) xs

-- | Create the final table layout.
createTable :: ShowOptions -- ^ command-line options
            -> Story       -- ^ story
            -> T.Text      -- ^ table layout
createTable options story = tabl EnvAscii hdecor vdecor aligns cells
  where
    hdecor  = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor  = DecorAll
    aligns  = [AlignLeft, AlignRight]
    cells   = ["Time", "Value"] : selectEntries entries (showOptCount options)
    entries = map (createEntry (showOptTimestamp options)) story

-- | Pretty-print the content of a story file.
perform :: ShowOptions -- ^ options
        -> IO ()       -- ^ command action
perform opts = do
  result <- storyLoad (showOptFile opts)
  case result of
    Left  err   -> T.putStrLn ("ERROR: " <> err)       >> exitFailure
    Right story -> T.putStrLn (createTable opts story) >> exitSuccess
