module SPEAC.Analysis where

import Types (Note (..), Notes, Time, sortByStart)

-- | Returns the music in beat-size chunks.
captureBeats :: Notes -> Time -> [Notes]
captureBeats music beat =
    collectBeats (sortByStart $ breakEventsIntoBeats beat music) beat

collectBeats :: Notes -> Time -> [Notes]
collectBeats clarifiedMusic beat = filter (not . null) $ loop clarifiedMusic beat
  where
    loop [] _ = []
    loop ns t = collected : loop ns' (t + beat)
      where
        collected = collectBeat t ns
        ns' = drop (length collected) ns

collectBeat :: Time -> Notes -> Notes
collectBeat beat = takeWhile ((< beat) . start)

breakEventsIntoBeats :: Time -> Notes -> Notes
breakEventsIntoBeats beat = concatMap (breakEvent beat)

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
