module Format
( Format(..)
, fmtChange
, fmtIdentify
) where

import qualified System.FilePath as F


-- | Swim files come in two formats: raw and compressed.
data Format
  = FmtZip -- ^ compressed content
  | FmtRaw -- ^ uncompressed content

-- | Deduce the format of a file based on its file extension.
fmtIdentify :: FilePath             -- ^ file name
            -> Either String Format -- ^ error | format
fmtIdentify path
  | ext == ".gs"  = Right FmtRaw
  | ext == ".gsz" = Right FmtZip
  | otherwise     = Left "unrecognized file extension"
  where ext = F.takeExtension path

-- | Apply a different file extension based on the format.
fmtChange :: Format   -- ^ format
          -> FilePath -- ^ old file name
          -> FilePath -- ^ new file name
fmtChange FmtRaw = flip F.replaceExtension ".gs"
fmtChange FmtZip = flip F.replaceExtension ".gsz"
