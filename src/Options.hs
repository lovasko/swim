module Options
( Options
, ShowOptions
, InfoOptions
, parser
) where

import qualified Command.Show.Options as CShow
import qualified Command.Info.Options as CInfo

data Command
  = CmdShow ShowOptions
  | CmdInfo InfoOptions

data Options = Options { optCommand :: Command }

commands = subparser
  $ command "show" 
 <> command "info" 
-- <> command "compress"
-- <> command "decompress"
-- <> command "check"
-- <> command "shift"
-- <> command "correlate"
-- <> command "export-csv"

optionsParser :: Parser Options -- ^ parser
optionsParser = Options <$> optionCommand

-- | Description of the utility.
optionsDesc :: InfoMod Options -- ^ parser description
optionsDesc = headerDesc <> fullDesc
  where headerDesc = header "GoatSwim - time series database combinators"

-- | Parser of the command-line options.
parser :: ParserInfo Options -- ^ parser
parser = info (helper <*> optionsParser) optionsDesc

-- | Perform a selected command.
performCommand :: Command -- ^ command
               -> IO ()   -- ^ action
performCommand (CmdShow opt) = CShow.perform opt
performCommand (CmdInfo opt) = CInfo.perform opt
