import Options.Applicative
import Options


main :: IO ()
main = do
  opts <- execParser Options.parser
  perform (optCommand opts)
