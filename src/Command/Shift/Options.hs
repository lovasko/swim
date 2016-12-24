module Command.Shift.Options
( ShitOptions(..)
, shiftOptDefine
) where

import Data.Word
import Options.Applicative


-- | Direction of the shifting.
data Direction = DirForward  -- ^ forward
               | DirBackward -- ^ backward

-- | Options of the "shift" command.
data ShiftOptions = ShiftOptions
  { shiftOptSeconds :: Word64
  , shiftOptMinutes :: Word64
  , shiftOptHours   :: Word64
  , shiftOptDays    :: Word64
  , shiftOptYears   :: Word64
  , shiftOptFile    :: FilePath }


-- | Compute the final shift (in seconds)
shiftSum :: ShiftOptions -- ^ command-line options
         -> Word64       -- ^ final shift
shiftSum (ShiftOptions sec min hr dy yr) = sum
  [ sec
  , min * 60
  , hr  * 3600
  , dy  * 86400
  , yr  * 31536000 ]

-- | TODO
optionSeconds :: Parser Word -- ^ parser
optionSeconds = option auto 
   $ long    "seconds"
  <> metavar "N"
  <> help    "TODO"

-- | TODO
optionMinutes :: Parser Word -- ^ parser
optionMinutes = option auto 
   $ long    "minutes"
  <> metavar "N"
  <> help    "TODO"

optionHours :: Parser Word -- ^ parser
optionHours = option auto 
   $ long    "minutes"
  <> metavar "N"
  <> help    "TODO"
