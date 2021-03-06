module Command.Show.Options
( ShowOptions(..)
, showOptDefine
) where

import Options.Applicative


-- | Options of the "show" command.
data ShowOptions = ShowOptions
  { showOptCount     :: Maybe Int
  , showOptTimestamp :: Bool
  , showOptFile      :: FilePath }
  deriving (Show)

-- | Resulting table entry count.
optionCount :: Parser (Maybe Int) -- ^ parser
optionCount = optional $ option auto
   $ short   'c'
  <> long    "count"
  <> metavar "COUNT"
  <> help    "Number of rendered data points"

-- | Switch to trigger timestamps instead of formatted dates.
optionTimestamp :: Parser Bool -- ^ parser
optionTimestamp = switch
   $ short   't'
  <> long    "timestamp"
  <> help    "Display time values as timestamps"

-- | Data file to render.
optionFile :: Parser FilePath -- ^ parser
optionFile = argument str (metavar "FILE")

-- | Command-line user interface.
showOptDefine :: Parser ShowOptions -- ^ parser
showOptDefine = ShowOptions <$> optionCount <*> optionTimestamp <*> optionFile
