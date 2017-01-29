module Command.Info.Options
( InfoOptions(..)
, infoOptDefine
) where

import Options.Applicative


-- | Options of the "info" command.
data InfoOptions = InfoOptions
  { infoOptTimestamp :: Bool
  , infoOptFile      :: FilePath }
  deriving (Show)

-- | Switch to trigger timestamps instead of formatted dates.
optionTimestamp :: Parser Bool -- ^ parser
optionTimestamp = switch
   $ short 't'
  <> long  "timestamp"
  <> help  "Display data point times as timestamps"

-- | Data file to render.
optionFile :: Parser FilePath -- ^ parser
optionFile = argument str (metavar "FILE")

-- | Command-line user interface.
infoOptDefine :: Parser InfoOptions -- ^ parser
infoOptDefine = InfoOptions <$> optionTimestamp <*> optionFile
