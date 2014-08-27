module SPEACAnalysis where

import Types (Note(..), Notes, Time)

captureBeats :: Notes -> Time -> [Notes]
captureBeats = undefined
  -- breakEventsIntoBeats
  -- collectBeats

breakEventsIntoBeats :: Time -> Notes -> Notes
breakEventsIntoBeats = undefined
  -- breakEvent

-- | Breaks the event if longer than a beat.
-- >>> breakEvent 8000 $ Note {pitch = 57, start = 7000, duration = 1000, channel = 3}
-- [Note {pitch = 57, start = 7000 % 1, duration = 1000 % 1, channel = 3}]
-- >>> breakEvent 1000 $ Note {pitch = 57, start = 7000, duration = 1500, channel = 3}
-- [Note {pitch = 57, start = 7000 % 1, duration = 1000 % 1, channel = 3},Note {pitch = 57, start = 8000 % 1, duration = 500 % 1, channel = 3}]
breakEvent :: Time -> Note -> Notes
breakEvent beat event@(Note {start = s, duration = d})
  | d <= beat = [event]
  | otherwise = event { duration = beat } : breakEvent beat event'
  where event' = event { start = s + beat
                       , duration = d - beat
                       }
