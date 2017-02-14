{-# LANGUAGE OverloadedStrings #-}

module Save
( storySave
) where

import Codec.Goat hiding (Story)
import qualified Data.ByteString as B
import qualified Data.Serialize as S

import Format
import Story
import Util


-- | Save a story into a file in the uncompressed format.
saveRaw :: Story        -- ^ story
        -> B.ByteString -- ^ binary encoded story
saveRaw story = S.runPut (mapM_ rule story)
  where rule = S.putTwoOf S.putWord32be S.putFloat32be

-- | Save a story into a file in the compressed format.
saveZip :: Story        -- ^ story
        -> B.ByteString -- ^ binary encoded story
saveZip story = S.runPut (S.put times >> S.put values)
  where
    times  = timeEncode  (map fst story)
    values = valueEncode (map snd story)

-- | Save a story into a file.
storySave :: Story    -- ^ story
          -> FilePath -- ^ file path
          -> IO ()    -- ^ action
storySave story path = case fmtIdentify path of
  Left  err    -> errorPrint err
  Right FmtRaw -> B.writeFile path (saveRaw story)
  Right FmtZip -> B.writeFile path (saveZip story)
