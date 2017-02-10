module Story
( Story    -- *
, apTimes  -- ([Word32] -> [Word32]) -> Story -> Story
, apValues -- ([Float]  ->  [Float]) -> Story -> Story
) where

import Data.Word


-- | Representation of a value change mapped over time.
type Story = [(Word32, Float)]

-- | Apply a function to all time points of the story.
apTimes :: ([Word32] -> [Word32]) -- ^ time chang
         -> Story                 -- ^ old story
         -> Story                 -- ^ new story
apTimes fn story = zip (fn $ map fst story) (map snd story)

-- | Apply a function to all value points of the story.
apValues :: ([Float] -> [Float]) -- ^ value change
         -> Story                -- ^ old story
         -> Story                -- ^ new story
apValues fn story = zip (map fst story) (fn $ map snd story)
