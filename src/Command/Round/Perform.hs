{-# LANGUAGE OverloadedStrings #-}

module Command.Round.Perform
( RoundOptions
, perform
, options
) where

import Data.Monoid
import Options.Applicative
import System.Exit
import qualified Data.Text.IO as T

import Load
import Save
import Story


-- | Floating-point rounding mode.
data RoundMode
  = RoundFair     -- ^ 0.4 = 0.0, 0.6 = 1.0
  | RoundFloor    -- ^ 0.4 = 0.0, 0.6 = 0.0
  | RoundCeiling  -- ^ 0.4 = 1.0, 0.6 = 1.0
  deriving (Eq)

data RoundOptions = RoundOptions
  { roundFile :: FilePath
  , roundMode :: RoundMode }
  deriving (Show)

instance Show RoundMode where
  show RoundFair    = "fair"
  show RoundFloor   = "floor"
  show RoundCeiling = "ceiling"

-- | Data file to augment.
optFile :: Parser FilePath -- ^ parser
optFile = argument str (metavar "FILE")

-- | Output format option.
optRoundMode :: Parser RoundMode -- ^ parser
optRoundMode = option (eitherReader modeReader)
   $ short   'm'
  <> long    "mode"
  <> value   RoundFair
  <> metavar "MODE"
  <> help    "Floating-point rounding mode, supported: fair, floor, ceiling"
  <> showDefault
  where
    modeReader "fair"    = Right RoundFair
    modeReader "floor"   = Right RoundFloor
    modeReader "ceiling" = Right RoundCeiling
    modeReader _         = Left "Rounding mode not supported"

-- | Command-line user interface.
options :: Parser RoundOptions -- ^ parser
options = RoundOptions <$> optFile <*> optRoundMode

-- | Perform the floating-point rounding operation.
applyMode :: RoundMode -- ^ rounding mode
          -> Float     -- ^ old number
          -> Integer   -- ^ new number
applyMode RoundFair    = round
applyMode RoundFloor   = floor
applyMode RoundCeiling = ceiling

-- | Peform story compression on a selected file.
perform :: RoundOptions -- ^ round options
        -> IO ()        -- ^ command action
--perform opts = case storyLoad file of
--  Left  err   -> T.putStrLn ("ERROR: " <> err) >> exitFailure
--  Right story -> storySave (change story) file       >> exitSuccess
--  where
--    file   = roundFile opts
--    mode   = roundMode opts
--    change = apValues (map (applyMode mode))

perform opts = storyLoad file >>= (\content -> case content of
  Left  err   -> T.putStrLn ("ERROR: " <> err) >> exitFailure
  Right story -> storySave (change story) file >> exitSuccess)
  where
    file   = roundFile opts
    mode   = roundMode opts
    change = apValues (map (fromIntegral . applyMode mode))