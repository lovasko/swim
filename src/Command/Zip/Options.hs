module Command.Zip.Options
( ZipOptions(..)
, parser
) where

import Options.Applicative


data ZipOptions = ZipOptions { zipOptFile :: FilePath }

