{-# LANGUAGE OverloadedStrings #-}

module Command.Info.Perform
( perform
) where

import Data.Maybe
import Data.Monoid
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


-- | Compute statistics on top of data point values.
valueStats :: [Float]    -- ^ values
           -> [[T.Text]] -- ^ table rows
valueStats [] = []
valueStats xs = [ ["Minimum", (textShow . fromJust . S.min)    xs]
                , ["Maximum", (textShow . fromJust . S.max)    xs]
                , ["Average", (textShow . fromJust . S.amean)  xs]
                , ["StdDev",  (textShow . fromJust . S.stddev) xs] ]

-- | Compute statistics on top of data point times.
timeStats :: InfoOptions
          -> [Word32]
          -> [[T.Text]]
timeStats _       []    = []
timeStats options times = [ ["Begin", timeBegin]
                          , ["End",   timeEnd]
                          , ["Size",  entryCount] ]
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
perform opts = do
  result <- storyLoad (infoOptFile opts)
  case result of
    Left  err   -> T.putStrLn ("ERROR: " <> err)       >> exitFailure
    Right story -> T.putStrLn (createTable opts story) >> exitSuccess
