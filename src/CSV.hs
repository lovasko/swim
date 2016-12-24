{-# LANGUAGE OverloadedStrings #-}

module CSV
( exportCSV
, importCSV
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Story
import Util


-- | Export all story data points into the CSV format.
exportCSV :: Story    -- ^ story
          -> Bool     -- ^ timestamp expansion
          -> FilePath -- ^ file path
          -> IO ()    -- ^ action
exportCSV story expandTS path = T.writeFile path content
  where
    content          = T.concatMap record story
    record (ts, val) = T.concat [showTime expandTS ts, ",", textShow val]

-- Import all story data points from the CSV format.
importCSV :: FilePath                 -- ^ file path 
          -> IO (Either T.Text Story) -- ^ error | story
importCSV path = fmap parse (T.readFile path)
  where
    parse text = 
