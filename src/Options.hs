module Options
( Options(..)
, parser
, perform
) where

import Options.Applicative
import qualified Command.Show.Options as CShow
import qualified Command.Show.Perform as CShow
import qualified Command.Info.Options as CInfo
import qualified Command.Info.Perform as CInfo


-- | Command-line subcommands.
data Command
  = CmdShow CShow.ShowOptions
  | CmdInfo CInfo.InfoOptions
  deriving (Show)

-- | Command-line options.
data Options = Options { optCommand :: Command }
  deriving (Show)

-- | Parsing of command-line subcommands.
commands :: Parser Command
commands = subparser
  $ command "show" (info (fmap CmdShow CShow.showOptDefine) fullDesc)
 <> command "info" (info (fmap CmdInfo CInfo.infoOptDefine) fullDesc)
-- <> command "compress"
-- <> command "decompress"
-- <> command "check"
-- <> command "shift"
-- <> command "correlate"
-- <> command "export-csv"

optionsParser :: Parser Options -- ^ parser
optionsParser = Options <$> commands --optionCommand

-- | Description of the utility.
optionsDesc :: InfoMod Options -- ^ parser description
optionsDesc = headerDesc <> fullDesc
  where headerDesc = header "swim - time series database combinators"

-- | Parser of the command-line options.
parser :: ParserInfo Options -- ^ parser
parser = info (helper <*> optionsParser) optionsDesc

-- | Perform a selected command.
perform :: Command -- ^ command
        -> IO ()   -- ^ action
perform (CmdShow opt) = CShow.perform opt
perform (CmdInfo opt) = CInfo.perform opt
