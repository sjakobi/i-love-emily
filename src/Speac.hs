module Speac where

import qualified Data.IntMap.Strict as M
import           Data.List          (foldl', sort, sortBy, zipWith4, (\\))
import           Data.Maybe         (fromJust, fromMaybe, listToMaybe)
import           Data.Ord           (comparing)
import qualified Data.Vector        as V

import           Types

type Interval = Int   -- Interval represents the number of semi-tones
                      -- in an interval
type Tension = Double

data Marked a = Unstarred a
              | Starred a
              deriving (Show)

{----------------------------------------------------------------------------
    Utility functions not used in the original Lisp code
-----------------------------------------------------------------------------}

-- | Return the object contained in a 'Marked'.
unmark :: Marked a -> a
unmark (Unstarred x) = x
unmark (Starred x) = x

-- | Returns the end of a note.
end :: Note -> Time
end n = start n + duration n

-- | Determine the interval between two pitches.
interval :: Pitch -> Pitch -> Interval
interval a b = abs $ a - b

-- | Helper function to convert from Cope's "event" type to 'Note'.
readNote :: (Time, Pitch, Time, Int, Int) -> Note
readNote (a, b, c, d, _) = Note { pitch = b
                                , start = a
                                , duration = c
                                , channel = d
                                }

-- | Sort notes chronologically
sortByStart :: Notes -> Notes
sortByStart = sortBy (comparing start)

-- | Return `True` if a 'Marked' object is 'Starred'.
isStarred :: Marked a -> Bool
isStarred (Starred _) = True
isStarred _           = False

{----------------------------------------------------------------------------
    Constants and data used throughout the code
-----------------------------------------------------------------------------}

-- | Returns the tension of any (positive) interval.
intervalTension :: Interval -> Tension
intervalTension = (table V.!) . (`rem` 12)
  where
    table = V.fromList [ 0      -- unison
                       , 1      -- minor second
                       , 0.8    -- major second
                       , 0.225  -- minor third
                       , 0.2    -- major third
                       , 0.55   -- perfect fourth
                       , 0.65   -- augmented fourth
                       , 0.1    -- perfect fifth
                       , 0.275  -- minor sixth
                       , 0.25   -- major sixth
                       , 0.7    -- minor seventh
                       , 0.9    -- major seventh
                       ]

-- | Is used once in 'breakAtEachEntrance'.
exitsAndEntrances :: Bool
exitsAndEntrances = False

-- | Looks up the relevant metric weight.
-- >>> metricTension 9 7
-- 5.0e-2
metricTension :: Int -> Int -> Tension
metricTension meter beatNumber =
    0.1 * fromIntegral beatNumber / fromIntegral invTension
  where
    invTension = fromJust (lookup meter metricTensionTable) !! pred beatNumber
    metricTensionTable :: [(Int, [Int])]
    metricTensionTable = [ (4, [2, 2, 6, 2])
                         , (2, [2, 2])
                         , (3, [2, 2, 2])
                         , (6, [2, 2, 2, 8, 4, 3])
                         , (9, [2, 2, 2, 8, 4, 3, 14, 8, 4])
                         ]

-- | Figure 7.12 from book.
bookExample :: Notes
bookExample = map readNote input
  where
    input = [ (0, 45, 1000, 4, 55), (0, 64, 1000, 3, 55)
            , (0, 69, 1000, 2, 55), (0, 73, 1000, 1, 55)
            , (1000, 57, 1000, 4, 55), (1000, 64, 1000, 3, 55)
            , (1000, 69, 1000, 2, 55), (1000, 73, 500, 1, 55)
            , (1500, 74, 500, 1, 55), (2000, 56, 1000, 4, 55)
            , (2000, 64, 1000, 3, 55), (2000, 71, 1000, 2, 55)
            , (2000, 76, 1000, 1, 55), (3000, 57, 1000, 4, 55)
            , (3000, 64, 1000, 3, 55), (3000, 69, 1000, 2, 55)
            , (3000, 73, 1000, 1, 55), (4000, 54, 1000, 4, 55)
            , (4000, 64, 500, 3, 55), (4500, 62, 500, 3, 5)
            , (4000, 69, 1000, 2, 55), (4000, 69, 1000, 1, 55)
            , (5000, 55, 1000, 4, 55), (5000, 62, 1000, 3, 55)
            , (5000, 67, 500, 2, 55), (5500, 66, 500, 2, 55)
            , (5000, 71, 1000, 1, 55), (6000, 57, 1000, 4, 55)
            , (6000, 57, 1000, 3, 55), (6000, 64, 1000, 2, 55)
            , (6000, 73, 1000, 1, 55), (7000, 50, 1000, 4, 55)
            , (7000, 57, 1000, 3, 55), (7000, 66, 1000, 2, 55)
            , (7000, 74, 1000, 1, 55)
            ]

-- | Interval, root placement, strength with 1 being strongest.
rootStrengthsAndRoots :: [(Interval, Int, Int)]
rootStrengthsAndRoots = []

{-
-- | This is the top-level for this SPEAC analysis program.
-- >>> runTheSpeacWeightings bookExample 4 8 4
-- [0.56,0.41,0.78,0.51,1.33,0.51,1.26,0.51]
runTheSpeacWeightings :: Notes -> Int -> Int -> Int -> [Tension]
runTheSpeacWeightings events beginBeat totalBeats meter =
    mapAdd verticalTensions metricTensions durationTensions approachTensions
  where
    verticalTensions = createListOfTensions $ collectPitchLists $ collectBeatLists $ breakAtEachEntrance events
    metricTensions = mapMetricTensions beginBeat totalBeats meter
    durationTensions = computeDurationTensions events
    approachTensions = getRootMotionWeightings events
-}

-- | Maps addition across the various parameters of the analysis.
mapAdd :: [Tension] -> [Tension] -> [Tension] -> [Tension] -> [Tension]
mapAdd = zipWith4 (\a b c d -> a + b + c + d)

-- | Maps the metric tensions in a given meter.
mapMetricTensions :: Int -> Int -> Int -> [Tension]
mapMetricTensions startBeat totalBeats meter = take totalBeats $
  map (metricTension meter) $ [startBeat..meter] ++ cycle [1..meter]

-- | Top-level function of the tension list creators.
-- >>> createListOfTensions $ collectPitchLists $ collectBeatLists $ breakAtEachEntrance bookExample
-- [0.30000000000000004,0.30000000000000004,0.5,0.30000000000000004,0.5,0.30000000000000004,0.30000000000000004,0.30000000000000004]
createListOfTensions :: [[[Pitch]]] -> [Tension]
createListOfTensions = map (minimum . rate . translateToIntervals)

-- | Collects beat lists.
collectBeatLists :: [[Marked Note]] -> [[[Marked Note]]]
collectBeatLists [] = []
collectBeatLists entranceLists =
    firstGroup : collectBeatLists (drop (length firstGroup) entranceLists)
  where
    firstGroup = groupBeats entranceLists

-- | Groups the beats together.
groupBeats :: [[Marked Note]] -> [[Marked Note]]
groupBeats [] = []
groupBeats (ns:nss)
  | any isStarred ns = ns : (groupBeats nss)
  | otherwise = [ns]

-- | Collects the pitches from its arg.
collectPitchLists :: [[[Marked Note]]] -> [[[Pitch]]]
collectPitchLists = map (map (map (pitch . unmark)))

-- | Translates groups of pitches into intervals.
-- >>> translateToIntervals [[73, 69, 64, 45]]
-- [[19,28]]
translateToIntervals :: [[Pitch]] -> [[Interval]]
translateToIntervals = map (intervalsToBassNote . removeOctaves . sort)
  where intervalsToBassNote (n:ns) = map (subtract n) ns
        intervalsToBassNote []     = error "Bad input"

-- | Translates its argument into weightings based on the stored values.
-- >>> rate [[7, 16]]
-- [0.30000000000000004]
rate :: [[Interval]] -> [Tension]
rate = map (sum . map intervalTension)

{----------------------------------------------------------------------------
    Break notes into non-overlapping groups of simultaneous events
-----------------------------------------------------------------------------}

-- | Breaks events at each new entrance.
--   The notes are broken into groups of notes that start at the same time.
--   Notes that are held while new notes begin are broken broken into
--   "subnotes" and appear in at least two groups. Notes that are continued
--   in the subsequent group are 'Starred', all others are 'Unstarred'.
--
--   Triplets (dt. Triolen) are treated specially (aligned?) in the
--   'fixTriplets' function.
--
-- >>> map (pitch . unmark) $ head $ breakAtEachEntrance bookExample
-- [73,69,64,45]
breakAtEachEntrance :: Notes -> [[Marked Note]]
breakAtEachEntrance = breakAtEachEntrance' . sortByStart . fixTheTriplets

-- | Worker function for 'breakAtEachEntrance'.
breakAtEachEntrance' :: Notes -> [[Marked Note]]
breakAtEachEntrance' [] = []
breakAtEachEntrance' orderedEvents@(e:_) =
    simultaneousEvents' : breakAtEachEntrance' orderedEvents'
  where
    simultaneousEvents' = map (resetDuration newEntranceTime) simultaneousEvents
    simultaneousEvents = sortBy (comparing channel) $ collectSimultaneousEvents orderedEvents
    newEntranceTime = if exitsAndEntrances
        then getNewExitAndEntranceTime orderedEvents (start e)
        else getNewEntranceTime orderedEvents (start e) (end e)
    -- Sorting shouldn't be necessary here. Any note in `continuingEvents`
    -- must start before or at the same time as the first notes in
    -- `remainingEvents`. I think...
    orderedEvents' = sortByStart continuingEvents ++ remainingEvents
    continuingEvents = filter ((> 0) . duration) $ map (resetNextDuration newEntranceTime) simultaneousEvents
    remainingEvents = orderedEvents \\ simultaneousEvents

-- | Removes the nils due to triplets.
-- >>> head $ fixTheTriplets bookExample
-- Note {pitch = 73, start = 0 % 1, duration = 1000 % 1, channel = 1}
fixTheTriplets :: Notes -> Notes
fixTheTriplets = concatMap fixTriplets . getAllChannels

-- | Fixes the triplet problem!?
--   Input: The chronologically sorted notes for a single channel.
fixTriplets :: Notes -> Notes
fixTriplets [] = []
fixTriplets [a] = [a]
fixTriplets (a:b:bs)
  | end a == start b =
      a : fixTriplets (b:bs)
  | end a `withinOne` start b =
      a {duration = start b - start a} : fixTriplets (b:bs)
  | otherwise =
      a : fixTriplets (b:bs)
  where
    withinOne :: Time -> Time -> Bool
    withinOne x y = abs (x - y) < 2

-- | Collects all channels in proper order.
--   Note: While Cope's 'get-all-channels' function returns a (possibly
--   empty) list of notes each for channels 1-16, this function returns
--   only those channels that are actually present in the input.
--
-- >>> head $ head $ getAllChannels bookExample
-- Note {pitch = 73, start = 0 % 1, duration = 1000 % 1, channel = 1}
getAllChannels :: Notes -> [Notes]
getAllChannels = map snd . M.toAscList . foldl' buildChannel M.empty
  where
    buildChannel :: M.IntMap Notes -> Note -> M.IntMap Notes
    buildChannel m n = M.insertWith (flip (++)) (channel n) [n] m

-- | Returns the time of the very next event, be it the end or the start of
--   a note.
getNewExitAndEntranceTime :: Notes -> Time -> Time
getNewExitAndEntranceTime orderedEvents startTime =
    getNewExitAndEntranceTime' newStartTime endTime
  where
    endTime = getShortestDuration startTime orderedEvents + startTime
    newStartTime = getNextStartTime startTime orderedEvents

-- | Worker function for 'getNewExitAndEntranceTime'
getNewExitAndEntranceTime' :: Maybe Time -> Time -> Time
getNewExitAndEntranceTime' (Just newStartTime) endTime
  | endTime > newStartTime = newStartTime
getNewExitAndEntranceTime' _ endTime = endTime

-- | Returns the shortest duration of all notes that start at `startTime`.
getShortestDuration :: Time -> Notes -> Time
getShortestDuration startTime =
    minimum . map duration . takeWhile ((== startTime) . start)

-- | Gets the start time of the first event starting after `startTime`.
-- >>> getNextStartTime 0 bookExample
-- Just (1000 % 1)
getNextStartTime :: Time -> Notes -> Maybe Time
getNextStartTime startTime = listToMaybe . filter (/= startTime) . map start

-- | Gets the new entrance time.
--   If there are any notes starting after `startTime`, return the start of
--   those. Else return `endTime`.
getNewEntranceTime :: Notes -> Time -> Time -> Time
getNewEntranceTime orderedEvents startTime endTime =
  fromMaybe endTime $ getNextStartTime startTime orderedEvents

-- | Resets the duration to not exceed the new entrance time and marks
--   the note with a star if it outlasts the new entrance time.
resetDuration :: Time -> Note -> Marked Note
resetDuration t n = star $ n { duration = newDuration }
  where
    star = if end n > t then Starred else Unstarred
    newDuration = if end n < t then duration n else t - start n

-- | Resets events to account for overlapped beats.
resetNextDuration :: Time -> Note -> Note
resetNextDuration newEntranceTime n =
    n { start = newEntranceTime
      , duration = end n - newEntranceTime
      }

-- | Return the first simultaneous notes in the input.
collectSimultaneousEvents :: Notes -> Notes
collectSimultaneousEvents [] = []
collectSimultaneousEvents ns@(n:_) = takeWhile ((== start n) . start) ns

{----------------------------------------------------------------------------
    Other stuff
-----------------------------------------------------------------------------}

-- | Removes all octave doublings.
-- >>> removeOctaves [60, 67, 64, 72]
-- [60,67,64]
removeOctaves :: [Pitch] -> [Pitch]
removeOctaves = foldl' addUnlessOctave []

-- | @'addUnlessOctave' ps p@ appends @p@ to @ps@ if @ps@ doesn't contain
-- an octave of @p@.
addUnlessOctave :: [Pitch] -> Pitch -> [Pitch]
addUnlessOctave ps p
  | any ((== 0) . (`rem` 12) . interval p) ps = ps
  | otherwise = ps ++ [p]

computeDurationTensions :: Notes -> [Tension]
computeDurationTensions = undefined

getRootMotionWeightings :: Notes -> [Tension]
getRootMotionWeightings = undefined

-- | Finds the motion between chord roots. For example:
-- >>> findMotionWeightings [45, 57, 64, 57, 62]
-- [0.0,0.1,0.1,0.55]
findMotionWeightings :: [Pitch] -> [Tension]
findMotionWeightings ps = map intervalTension $ zipWith interval ps (drop 1 ps)

{-
-- | Returns the chord roots of arg.
-- >>> getChordRoots bookExample
-- [45,57,64,57,69,55,57,50]
getChordRoots :: Notes -> [Pitch]
getChordRoots = undefined
-}
