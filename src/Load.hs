module Load
( storyLoad
) where

import Codec.Goat hiding (Story)
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.Serialize as S

import Format
import Story


-- | Load a story from a file in the uncompressed format.
loadRaw :: B.ByteString        -- ^ file content
        -> Either String Story -- ^ error | story
loadRaw bs = S.runGet (many $ S.getTwoOf S.getWord32be S.getFloat32be) bs

-- | Load a story from a file in the compressed format.
loadZip :: B.ByteString        -- ^ file content
        -> Either String Story -- ^ error | story
loadZip bs = case S.runGet (S.getTwoOf S.get S.get) bs of
  Left  err      -> Left  err
  Right (ts, vs) -> Right $ zip (timeDecode ts) (valueDecode vs)

-- | Load any story file into memory.
storyLoad :: FilePath                 -- ^ file path
          -> IO (Either String Story) -- ^ error | story
storyLoad path = do
  content <- B.readFile path
  return $ case fmtIdentify path of
    Left  err    -> Left err
    Right FmtRaw -> loadRaw content
    Right FmtZip -> loadZip content
