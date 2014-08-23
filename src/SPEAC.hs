module SPEAC where

import qualified Data.IntMap.Strict as M
import           Data.List          (foldl', minimumBy, nub, sort, sortBy,
                                     zipWith4, (\\))
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
rootStrengthAndRoot :: Interval -> (Interval, Int, Int)
rootStrengthAndRoot i = head $ filter (\(a, _, _) -> a == i)
    [ (7, 0, 1), (5, 5, 2), (4, 0, 3), (8, 8, 4), (3, 0, 5), (9, 9, 6)
    , (2, 2, 7), (10, 0, 8), (1, 1, 9), (11, 0, 10), (0, 0, 11), (6, 6, 12)
    ]

{----------------------------------------------------------------------------
    Main function for SPEAC
-----------------------------------------------------------------------------}

-- | This is the top-level for this SPEAC analysis program.
-- >>> map myRound $ runTheSPEACWeightings bookExample 4 8 4
-- [0.56,0.41,0.78,0.51,1.33,0.51,1.26,0.51]
runTheSPEACWeightings :: Notes -> Int -> Int -> Int -> [Tension]
runTheSPEACWeightings events beginBeat totalBeats meter =
    mapAdd verticalTensions metricTensions durationTensions approachTensions
  where
    verticalTensions = createListOfTensions
                     $ collectPitchLists
                     $ collectBeatLists
                     $ breakAtEachEntrance events
    metricTensions = mapMetricTensions beginBeat totalBeats meter
    durationTensions = computeDurationTensions events
    approachTensions = getRootMotionWeightings events

-- | Maps addition across the various parameters of the analysis.
mapAdd :: [Tension] -> [Tension] -> [Tension] -> [Tension] -> [Tension]
mapAdd = zipWith4 (\a b c d -> a + b + c + d)

-- | Maps the metric tensions in a given meter.
mapMetricTensions :: Int -> Int -> Int -> [Tension]
mapMetricTensions startBeat totalBeats meter = take totalBeats $
  map (metricTension meter) $ [startBeat..meter] ++ cycle [1..meter]

{----------------------------------------------------------------------------
    Utilities to compute vertical tension
-----------------------------------------------------------------------------}

-- | Top-level function of the tension list creators.
-- >>> createListOfTensions $ collectPitchLists $ collectBeatLists $ breakAtEachEntrance bookExample
-- [0.3,0.3,0.5,0.3,0.5,0.3,0.3,0.3]
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
  | any isStarred ns = ns : groupBeats nss
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

-- | Translates its argument into weightings based on the stored values.
-- >>> rate [[7, 16]]
-- [0.3]
rate :: [[Interval]] -> [Tension]
rate = map (myRound . sum . map intervalTension)

-- Round to two decimals.
-- >>> myRound 1.01111
-- 1.01
myRound :: Double -> Double
myRound f = fromInteger (round $ f * 100) / 100.0

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
    simultaneousEvents' =
      map (resetDuration newEntranceTime) simultaneousEvents
    simultaneousEvents =
      sortBy (comparing channel) $ collectSimultaneousEvents orderedEvents
    newEntranceTime = if exitsAndEntrances
        then getNewExitAndEntranceTime orderedEvents (start e)
        else getNewEntranceTime orderedEvents (start e) (end e)
    -- Sorting shouldn't be necessary here. Any note in `continuingEvents`
    -- must start before or at the same time as the first notes in
    -- `remainingEvents`. I think...
    orderedEvents' = sortByStart continuingEvents ++ remainingEvents
    continuingEvents = filter ((> 0) . duration) $
        map (resetNextDuration newEntranceTime) simultaneousEvents
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
    Utilities to compute duration tension
-----------------------------------------------------------------------------}

-- | Computes the tensions for events.
-- >>> computeDurationTensions bookExample
-- [6.0e-2,6.0e-2,8.0e-2,6.0e-2,8.0e-2,6.0e-2,6.0e-2,6.0e-2]
computeDurationTensions :: Notes -> [Tension]
computeDurationTensions events =
    zipWith addTensions durations intervalTensions
  where
    addTensions d i = myRound $ 0.1 * fromRational d / 4000 + 0.1 * i
    durations = durationMap $ collectBeatLists $ breakAtEachEntrance events
    intervalTensions = createListOfTensions
                     $ collectPitchLists
                     $ collectBeatLists
                     $ breakAtEachEntrance events

-- | Maps the duration per beat.
durationMap :: [[[Marked Note]]] -> [Time]
durationMap = getDurations . map (start . unmark . head . head)

-- | Returns the time intervals between `ontimes`.
getDurations :: [Time] -> [Time]
getDurations ontimes = getDurations' ontimes 0

getDurations' :: [Time] -> Time -> [Time]
getDurations' (t:u:us)  _    = d : getDurations' (u:us) d
  where d = u - t
getDurations' _         prev = [prev]

{----------------------------------------------------------------------------
    Utilities to compute approach tension
-----------------------------------------------------------------------------}

-- | Returns the tensions due to root motions.
--
--   The function behaves as expected in the book and in speac.lisp, line 109:
-- >>> getRootMotionWeightings bookExample
-- [0.0,0.0,0.1,0.1,0.55,0.1,0.8,0.1]
--
--   In contrast, line 610 in speac.lisp demands the following behavior:
-- >> getRootMotionWeightings bookExample
-- [0.0,0.0,0.1.0.0,0.8,0.8,0.1]
--
--   Clearly, the function can satisfy only one of these expectations.
getRootMotionWeightings :: Notes -> [Tension]
getRootMotionWeightings =
    -- The zero here is to account for the first chord not having an
    -- approach.
    (0 :) . findMotionWeightings . getChordRoots

-- | Finds the motion between chord roots. For example:
-- >>> findMotionWeightings [45, 57, 64, 57, 62, 55, 57, 50]
-- [0.0,0.1,0.1,0.55,0.1,0.8,0.1]
findMotionWeightings :: [Pitch] -> [Tension]
findMotionWeightings ps = map intervalTension
                        $ zipWith interval ps (drop 1 ps)

-- | Returns the chord roots of arg.
--
--   This function does not satisfy the expected behavior as listed in
--   speac.list on line 634:
-- >> getChordRoots bookExample
-- [45,57,64,57,69,55,57,50]
--
--   Yet it does contribute to the correct behavior of
--   'runTheSpeacWeightings', so it's probably fine.
getChordRoots :: Notes -> [Pitch]
getChordRoots events = zipWith getChordRoot roots onBeatPitchLists
  where onBeatPitchLists = map (sort . nub . concat)
                         $ collectPitchLists
                         $ collectBeatLists
                         $ breakAtEachEntrance events
        roots = map (findStrongestRootInterval . derive) onBeatPitchLists

getChordRoot :: Interval -> [Pitch] -> Pitch
getChordRoot r = findUpperLower r . fromJust . findIntervalInChord r

-- |
-- >>> derive [55, 64, 84]
-- [0,5,8,9]
-- >>> derive [41, 74, 76]
-- [0,2,9,11]
derive :: [Pitch] -> [Interval]
derive = sort . nub . deriveAllIntervals

-- | Returns the root note.
-- >>> findUpperLower 7 (45, 64)
-- 45
findUpperLower :: Interval -> (Pitch, Pitch) -> Pitch
findUpperLower root (a, b)
  | r == 0    = a
  | otherwise = b
  where (_, r, _) = rootStrengthAndRoot root

-- | Returns the strongest root interval.
-- >>> findStrongestRootInterval [0, 7, 4, 0, 0, 9, 5, 0, 8]
-- 7
findStrongestRootInterval :: [Interval] -> Interval
findStrongestRootInterval =
    minimumBy (comparing ((\(_, _, s) -> s) . rootStrengthAndRoot))

-- | Derive all possible intervals from pitches.
-- >>> deriveAllIntervals [45, 64, 69, 73]
-- [0,7,0,4,0,5,9,0,4,0]
--
-- Note: This example result contains one more zero than the example in the
-- original Lisp code but seems to be correct.
deriveAllIntervals :: [Pitch] -> [Interval]
deriveAllIntervals pitches@(_:p:ps) =
    deriveIntervals pitches ++ deriveAllIntervals (p:ps)
deriveAllIntervals _ = [0]

-- |
-- >>> deriveIntervals [45, 64, 69, 73]
-- [0,7,0,4]
deriveIntervals :: [Pitch] -> [Interval]
deriveIntervals pitches@(p:_) = map ((`mod` 12) . subtract p) pitches
deriveIntervals [] = []

-- | Returns the interval in the chord.
-- >>> findIntervalInChord 7 [54, 62, 64, 69]
-- Just (69,62)
findIntervalInChord :: Interval -> [Pitch] -> Maybe (Pitch, Pitch)
findIntervalInChord i chord =
    findItInChord i $ deriveAllPitches chord

-- | Finds the interval in chord
-- >>> findItInChord 7 [54, 54, 54, 62, 54, 64, 54, 69, 62, 62, 62, 64, 62, 69, 64, 64, 64, 69]
-- Just (69,62)
findItInChord :: Interval -> [Pitch] -> Maybe (Pitch, Pitch)
findItInChord _ [] = Nothing
findItInChord i ps = listToMaybe
                   $ filter (\(a, b) -> (a - b) `mod` 12 == i)
                   $ zip (tail ps) ps

-- | Derives all of the pitches.
-- >>> deriveAllPitches [54, 62, 64, 69]
-- [54,54,54,62,54,64,54,69,62,62,62,64,62,69,64,64,64,69]
deriveAllPitches :: [Pitch] -> [Pitch]
deriveAllPitches pitches@(_:b:bs) =
    concat (derivePitches pitches) ++ deriveAllPitches (b:bs)
deriveAllPitches _ = []

-- |
-- >>> derivePitches [54, 62, 64, 69]
-- [[54,54],[54,62],[54,64],[54,69]]
derivePitches :: [Pitch] -> [[Pitch]]
derivePitches []            = []
derivePitches pitches@(p:_) = [[p, q] | q <- pitches]

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
