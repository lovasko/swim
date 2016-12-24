module Command.Edit.Perform
( EditOptions(..)
, perform
) where

import Data.Maybe
import qualified System.Posix.Env as P
import qualified System.Posix.Temp as P


-- | Get the preferred editor. If no editor is set, "vi" is used as the
-- default.
getEditor :: EditOptions -- ^ command-line options
          -> IO String   -- ^ editor name
getEditor options = do
  let optEditor = editOptEditor options
  envEditor     <- P.getEnv "EDITOR"
  return $ head $ catMaybes [optEditor, envEditor, Just "vi"]

-- | Edit the data points in text editor.
perform :: EditOptions
        -> IO ()
perform options = do
