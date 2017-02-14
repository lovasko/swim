module Util
( bool
, errorPrint
, showTime
, textShow
) where

import Data.Monoid
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- | Print an error message.
errorPrint :: String -- ^ error message
           -> IO ()  -- ^ print action
errorPrint err = T.putStrLn (T.pack "ERROR: " <> T.pack err)

-- | Convert any Showable type into a Text instance.
textShow :: (Show a)
         => a      -- ^ any type
         -> T.Text -- ^ textual representation
textShow = (T.pack . show)

-- | If/else construct.
bool :: a    -- ^ True option
     -> a    -- ^ False option
     -> Bool -- ^ bool
     -> a    -- ^ result
bool x _ True  = x
bool _ y False = y

-- | Convert a timestamp to a human-readable form.
showTime :: Bool   -- ^ timestamp expansion
         -> Word32 -- ^ timestamp
         -> T.Text -- ^ textual representation
showTime True  ts = textShow ts
showTime False ts = T.pack (formatTime defaultTimeLocale "%Y-%b-%d %T" ts')
  where ts' = posixSecondsToUTCTime (fromIntegral ts + 946684800)
