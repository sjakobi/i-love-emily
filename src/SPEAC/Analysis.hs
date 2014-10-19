module SPEAC.Analysis where

import Data.Maybe (listToMaybe)

import SPEAC.Tensions (Tension)
import Types (Note (..), Notes, SpeacLabel (..), Time, sortByStart)

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

-- |
-- >>> runSpeac [0.56, 0.41, 0.78, 0.51, 1.33, 0.51, 1.26, 0.51] 0.73
-- [Preparation,Extension,Statement,Extension,Antecedent,Consequent,Antecedent,Consequent]
runSpeac :: [Tension] -> Tension -> [SpeacLabel]
runSpeac weights average =
  developSpeac weights average (maximum weights) (minimum weights)

-- |
-- >>> developSpeac [0.56, 0.41, 0.78, 0.51, 1.33, 0.51, 1.26, 0.51] 0.73 1.33 0.41
-- [Preparation,Extension,Statement,Extension,Antecedent,Consequent,Antecedent,Consequent]
developSpeac :: [Tension] -> Tension -> Tension -> Tension -> [SpeacLabel]
developSpeac weights average largest smallest =
    go weights Nothing Nothing
  where
    go []     _              _                  = []
    go (w:ws) previousWeight previousAssignment =
        assignment : go ws (Just w) (Just previousAssignment')
      where
        (assignment, previousAssignment')
          | withinPointTwo w previousWeight
          = (Extension, Extension)
          | withinPointTwo w (listToMaybe ws) 
          = assignments Preparation Extension False
          | withinPointTwo w (Just average)
          = assignments Statement Extension True
          | withinPointTwo w (Just largest)
          = assignments Antecedent Extension False
          | (Just Antecedent) <- previousAssignment
          , withinPointTwo w (Just smallest)
          = (Consequent, Consequent)
          | otherwise
          = assignments Statement Extension True
        assignments a b aIsPrev = (ass, prev)
          where ass = if previousAssignment == (Just a) then b else a
                prev = if aIsPrev then a else ass

withinPointTwo :: Tension -> Maybe Tension -> Bool
withinPointTwo _     Nothing       = False
withinPointTwo first (Just second) = abs (first - second) < 0.2
