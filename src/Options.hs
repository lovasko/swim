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
import qualified Command.Round.Perform as CRound
import qualified Command.Compress.Perform as CCompress
import qualified Command.Decompress.Perform as CDecompress


-- | Command-line subcommands.
data Command
  = CmdShow  CShow.ShowOptions
  | CmdInfo  CInfo.InfoOptions
  | CmdRound CRound.RoundOptions
  | CmdCompress CCompress.CompressOptions
  | CmdDecompress CDecompress.DecompressOptions
  deriving (Show)

-- | Command-line options.
data Options = Options { optCommand :: Command }
  deriving (Show)

-- | Parsing of command-line subcommands.
commands :: Parser Command
commands = subparser
  $ command "show" (info (fmap CmdShow CShow.showOptDefine) fullDesc)
 <> command "info" (info (fmap CmdInfo CInfo.infoOptDefine) fullDesc)
 <> command "round" (info (fmap CmdRound CRound.options) fullDesc)
 <> command "compress" (info (fmap CmdCompress CCompress.options) fullDesc)
 <> command "decompress" (info (fmap CmdDecompress CDecompress.options) fullDesc)
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
perform (CmdRound opt) = CRound.perform opt
perform (CmdCompress opt) = CCompress.perform opt
perform (CmdDecompress opt) = CDecompress.perform opt
