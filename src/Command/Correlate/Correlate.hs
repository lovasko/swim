module Command.Correlate
( perform
) where

-- IMPORTANT: this has to complain if the two stories do not overlap enough

import Stats
import Load

findOverlap :: [(Word32, Float)]
            -> [(Word32, Float)]
            -> ([Float], [Float])

perform :: MasterOptions
        -> CorrelateOptions
        -> IO ()
perform options = do
  story1 = load (getFirstStory options)
  story2 = load (getSecondStory options)
  let (xs, ys) = overlap story1 story2
  T.putStrLn $ show $ Stats.pearson xs ys
