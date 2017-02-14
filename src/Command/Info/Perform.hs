{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Info.Perform
( perform
) where

import Data.Function
import Data.List
import Data.Maybe
import Data.Word
import System.Exit
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Stats.Foldable as S

import Command.Info.Options
import Load
import Story
import Util


-- | Produce a human-friendly percentage notation of an integer. Percentages
-- lesser than 1.0 are replaced with "<1".
computePercent :: Int    -- ^ full length
               -> Int    -- ^ count
               -> T.Text -- ^ textual representation
computePercent _   0 = "N/A"
computePercent len cnt
  | x < 1.0   = "<1"
  | otherwise = textShow (floor x :: Int)
  where x = (fromIntegral cnt) / (fromIntegral len) * 100.0 :: Float

-- | Find three elements of a list with the highest occurrence.
top3 :: Ord a
     => [a]        -- ^ list
     -> [(a, Int)] -- ^ element & number of occurrences
top3 xs = take 3 processed
  where
    processed = reverse $ map (\x -> (head x, length x)) groups
    groups    = sortBy (compare `on` length) . group . sort $ xs

-- | Find the three intervals in the time points with highest occurrence,
-- along with the percentage to the whole story.
findIntervals :: [Word32] -- ^ time points
              -> T.Text   -- ^ intervals information
findIntervals []  = "N/A"
findIntervals [_] = "N/A"
findIntervals ts  = T.intercalate ", " $ map conv (top3 deltas)
  where
    conv (val, cnt) = T.concat [textShow val, "(", percent cnt, "%)"]
    percent         = computePercent (length ts)
    deltas          = zipWith (-) (tail ts) ts

-- | Compute statistics on top of data point values.
valueStats :: [Float]    -- ^ values
           -> [[T.Text]] -- ^ table rows
valueStats [] = []
valueStats xs =
  [ ["Range",   range]
  , ["Average", (textShow . fromJust . S.amean)  xs]
  , ["StdDev",  (textShow . fromJust . S.stddev) xs]
  , ["#Unique", (textShow . length   . nub)      xs] ]
  where
    range = T.unwords [low, "~~", high]
    low   = (textShow . fromJust . S.min) xs
    high  = (textShow . fromJust . S.max) xs

-- | Compute statistics on top of data point times.
timeStats :: InfoOptions
          -> [Word32]
          -> [[T.Text]]
timeStats _       []    = []
timeStats options times =
  [ ["Begin",     timeBegin]
  , ["End",       timeEnd]
  , ["#Entries",  entryCount]
  , ["Intervals", findIntervals times] ]
  where
    timeBegin  = showTime (infoOptTimestamp options) (head times)
    timeEnd    = showTime (infoOptTimestamp options) (last times)
    entryCount = (textShow . length) times

-- | Create table rows with information about the data points.
createTable :: InfoOptions
            -> Story
            -> T.Text
createTable _       []    = T.empty
createTable options story = tabl EnvAscii hdecor vdecor aligns cells
  where
    hdecor = DecorAll
    vdecor = DecorAll
    aligns = repeat AlignRight
    cells  = times ++ values
    times  = timeStats options (map fst story)
    values = valueStats (map snd story)

-- | Print information about the data points stored in a file.
perform :: InfoOptions -- ^ command-line options
        -> IO ()       -- ^ action
perform opts = storyLoad (infoOptFile opts) >>= \case
  Left  err   -> errorPrint err                      >> exitFailure
  Right story -> T.putStrLn (createTable opts story) >> exitSuccess
