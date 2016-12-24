module Commands.Check
( perform
) where

import qualified Data.Text.IO as T

import Load

checkValue :: [Float]
           -> IO ()
checkValue = mapM_ 

checkTimes :: [Word32]
           -> IO ()

find


perform :: CheckOptions -- ^ options
        -> IO ()        -- ^ action
perform options = do
  story <- load (checkGetFile options)
  checkTimes (map fst story)
  checkValues (map snd story)
  
