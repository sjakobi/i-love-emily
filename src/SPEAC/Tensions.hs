module SPEAC.Tensions where

import qualified Data.Array         as A
import           Data.Function      (on)
import qualified Data.IntMap.Strict as M
import           Data.List          (find, foldl', minimumBy, nub, nubBy,
                                     sortBy, zipWith4)
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Data.Ord           (comparing)
import qualified Data.Vector        as V

import           Internal.Utils     (pairings, spanPlus, unfoldListUntilEmpty,
                                     (.:))
import           Types

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
    invTension = (metricTensionTable A.! meter) A.! beatNumber
    metricTensionTable =
      A.array (2, 9) $ map (\(a, bs) -> (a, A.listArray (1, length bs) bs))
          [ (4, [2, 2, 6, 2])
          , (2, [2, 2])
          , (3, [2, 2, 2])
          , (6, [2, 2, 2, 8, 4, 3])
          , (9, [2, 2, 2, 8, 4, 3, 14, 8, 4 :: Int])
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

-- | Return the strength of the interval formed by two pitches, 1 being
--   the maximum strength, 12 the minimum.
--
--   See the linked paragraph for an explanation:
--   http://en.wikipedia.org/wiki/Harmonic_series_(music)#Interval_strength
intervalStrength :: Pitch -> Pitch -> Int
intervalStrength =  (strengths A.!) .: interval
  where strengths = A.array (0, 11)
          [ (7, 1), (5, 2), (4, 3), (8, 4), (3, 5), (9, 6)
          , (2, 7), (10, 8), (1, 9), (11, 10), (0, 11), (6, 12)
          ]

-- | Return the root of the interval formed by two pitches.
--
-- See http://en.wikipedia.org/wiki/Interval_(music)#Interval_root
-- for an explanation.
intervalRoot :: Pitch -> Pitch -> Pitch
intervalRoot a b
  | interval a b `elem` topRoots = max a b
  | otherwise                    = min a b
  where
    topRoots = [7, 4, 3, 10, 11, 0]

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
    verticalTensions = createListOfTensions pitchLists
    metricTensions = mapMetricTensions beginBeat totalBeats meter
    -- In Cope's original code the addition of the vertical tensions (scaled
    -- by a factor of 0.1) and the duration tensions happens already in the
    -- compute-duration-tensions function.
    durationTensions = zipWith (\v d -> myRound $ 0.1 * v + d) verticalTensions
                     $ computeDurationTensions' beatLists
    approachTensions = getRootMotionWeightings' pitchLists
    pitchLists = collectPitchLists beatLists
    beatLists = collectBeatLists $ breakAtEachEntrance events

-- | Maps addition across the various parameters of the analysis.
mapAdd :: [Tension] -> [Tension] -> [Tension] -> [Tension] -> [Tension]
mapAdd = zipWith4 (\a b c d -> a + b + c + d)

-- | Collects beat lists.
collectBeatLists :: [[Marked Note]] -> [[[Marked Note]]]
collectBeatLists = unfoldListUntilEmpty (spanPlus (any isStarred))

-- | Collects the pitches from its arg.
collectPitchLists :: [[[Marked Note]]] -> [[[Pitch]]]
collectPitchLists = map (map (map (pitch . unmark)))

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
breakAtEachEntrance = unfoldListUntilEmpty breakAtEachEntrance'
                    . sortByStart
                    . fixTheTriplets

-- | Worker function for 'breakAtEachEntrance'.
breakAtEachEntrance' :: Notes -> ([Marked Note], Notes)
breakAtEachEntrance' [] = ([], [])
breakAtEachEntrance' orderedEvents@(e:_) = (simultaneousEvents', orderedEvents')
  where
    simultaneousEvents' = map (resetDuration newEntranceTime)
                        $ sortBy (comparing channel) simultaneousEvents
    orderedEvents' = continuingEvents ++ remainingEvents
    newEntranceTime = if exitsAndEntrances
        then getNewExitAndEntranceTime orderedEvents (start e)
        else getNewEntranceTime orderedEvents (start e) (end e)
    continuingEvents =
        mapMaybe (resetNextDuration' newEntranceTime) simultaneousEvents
    (simultaneousEvents, remainingEvents) =
        span ((== start e) . start) orderedEvents

-- | Removes the nils due to triplets.
-- >>> head $ fixTheTriplets bookExample
-- Note {pitch = 73, start = 0 % 1, duration = 1000 % 1, channel = 1}
fixTheTriplets :: Notes -> Notes
fixTheTriplets = concatMap fixTriplets . getAllChannels

-- | Fixes the triplet problem for a single channel of subsequent notes.
--   Any note that doesn't end at the start of its successor but within less
--   than 2 time units has its duration reset so that it will end exactly at the
--   beginning of the next note.
fixTriplets :: Notes -> Notes
fixTriplets [] = []
fixTriplets [a] = [a]
fixTriplets (a:b:bs)
  | end a == start b          = a : bbs'
  | end a `withinOne` start b = a {duration = start b - start a} : bbs'
  | otherwise                 = a : bbs'
  where
    withinOne x y = abs (x - y) < 2
    bbs' = fixTriplets (b:bs)

-- | Collects all channels in proper order.
--
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
getNewExitAndEntranceTime orderedEvents startTime
  | (Just t) <- newStartTime, t < endTime = t
  | otherwise                             = endTime
  where
    newStartTime = getNextStartTime' startTime orderedEvents
    endTime = getShortestDuration startTime orderedEvents + startTime

-- | Returns the shortest duration of all notes that start at `startTime`.
getShortestDuration :: Time -> Notes -> Time
getShortestDuration startTime =
    minimum . map duration . takeWhile ((== startTime) . start)

-- | Gets the start time of the first event starting after `startTime`.
-- >>> getNextStartTime' 0 bookExample
-- Just (1000 % 1)
getNextStartTime' :: Time -> Notes -> Maybe Time
getNextStartTime' startTime = find (/= startTime) . map start

-- | Gets the new entrance time.
--   If there are any notes starting after `startTime`, return the start of
--   those. Else return `endTime`.
getNewEntranceTime :: Notes -> Time -> Time -> Time
getNewEntranceTime orderedEvents startTime endTime =
  fromMaybe endTime $ getNextStartTime' startTime orderedEvents

-- | Resets the duration to not exceed the new entrance time and marks
--   the note with a star if it outlasts the new entrance time.
resetDuration :: Time -> Note -> Marked Note
resetDuration t n = star $ n { duration = newDuration }
  where
    star = if end n > t then Starred else Unstarred
    newDuration = if end n < t then duration n else t - start n

-- | Reset an event depending on how long it lasts.
-- If the event lasts while new events begin, its start and duration is
-- reset and it is returned with `Just`. Otherwise, the function returns
-- `Nothing`.
resetNextDuration' :: Time -> Note -> Maybe Note
resetNextDuration' newEntranceTime n
  | end n <= newEntranceTime = Nothing
  | otherwise                = Just $ n { start = newEntranceTime
                                        , duration = end n - newEntranceTime
                                        }

{----------------------------------------------------------------------------
    Utilities to compute vertical tension
-----------------------------------------------------------------------------}

-- | Top-level function of the tension list creators.
--
-- Note: This function is named create-listS-of-tensions in the original
-- sources. The current naming seems more appropriat though.
--
-- >>> createListOfTensions $ collectPitchLists $ collectBeatLists $ breakAtEachEntrance bookExample
-- [0.3,0.3,0.5,0.3,0.5,0.3,0.3,0.3]
createListOfTensions :: [[[Pitch]]] -> [Tension]
createListOfTensions = map (minimum . map (rate' . translateToIntervals'))

-- | Returns the unique (modulo 12) intervals between the input pitches and
-- their lowest note.
-- >>> translateToIntervals' [73, 69, 64, 45]
-- [28,19]
translateToIntervals' :: [Pitch] -> [Interval]
translateToIntervals' = intervalsToBassNote . removeOctaves . prependBassNote
  where intervalsToBassNote (n:ns) = map (subtract n) ns
        intervalsToBassNote []     = error "Impossible!"
        prependBassNote ns = minimum ns : ns

-- | Removes all octave doublings.
-- >>> removeOctaves [60, 67, 64, 72]
-- [60,67,64]
removeOctaves :: [Pitch] -> [Pitch]
removeOctaves = nubBy ((==) `on` (`rem` 12))

-- | Returns the rounded sum of the tensions for the input intervals.
-- >>> rate' [7, 16]
-- 0.3
rate' :: [Interval] -> Tension
rate' = myRound . sum . map intervalTension

-- | Round to two decimals.
-- >>> myRound 1.01111
-- 1.01
myRound :: Double -> Double
myRound f = fromInteger (round $ f * 100) / 100.0

{----------------------------------------------------------------------------
    Metric tension computation
-----------------------------------------------------------------------------}

-- | Maps the metric tensions in a given meter.
mapMetricTensions :: Int -> Int -> Int -> [Tension]
mapMetricTensions startBeat totalBeats meter = take totalBeats $
  map (metricTension meter) $ [startBeat..meter] ++ cycle [1..meter]

{----------------------------------------------------------------------------
    Utilities to compute duration tension
-----------------------------------------------------------------------------}

-- | Computes duration tensions.
--
-- Note: In the original Lisp code, interval tensions are added to the
-- duration tensions with a factor of 0.1. In this program, the addition
-- happens in the top-level runTheSPEACWeightings function.
-- Because of this change, the the returned tensions are lower than those
-- produced by the original Lisp function.
computeDurationTensions' :: [[[Marked Note]]] -> [Tension]
computeDurationTensions' = map (fromRational . (/ 40000)) . durationMap

-- | Maps the duration per beat.
durationMap :: [[[Marked Note]]] -> [Time]
durationMap = getDurations . map (start . unmark . head . head)

-- | Returns the time intervals between `ontimes`.
-- >>> getDurations $ map (* 1000) [0..3]
-- [1000 % 1,1000 % 1,1000 % 1,1000 % 1]
getDurations :: [Time] -> [Time]
getDurations ontimes = ds ++ [last ds]
  where ds = zipWith (-) (drop 1 ontimes) ontimes

{----------------------------------------------------------------------------
    Utilities to compute approach tension
-----------------------------------------------------------------------------}

-- | Returns the tensions due to root motions.
--
--   This function is similar to the get-root-motion-weightings function
--   in Cope's original code but takes its input at a further stage of
--   processing than the original.
--
--   Its output is as expected in the book and in speac.lisp, line 109:
-- >>> getRootMotionWeightings' $ collectPitchLists $ collectBeatLists $ breakAtEachEntrance $ bookExample
-- [0.0,0.0,0.1,0.1,0.55,0.1,0.8,0.1]
--
--   It should be noted that speac.lisp specifies a different behavior
--   around line 610:
-- > getRootMotionWeightings bookExample
-- [0.0,0.0,0.1.0.0,0.8,0.8,0.1]
--
--  This behavior is not satisfied.
getRootMotionWeightings' :: [[[Pitch]]] -> [Tension]
getRootMotionWeightings' =
    -- The zero here is to account for the first chord not having an
    -- approach.
    (0 :) . findMotionWeightings . map getChordRoot

-- | Finds the motion between chord roots. For example:
-- >>> findMotionWeightings [45, 57, 64, 57, 62, 55, 57, 50]
-- [0.0,0.1,0.1,0.55,0.1,0.8,0.1]
findMotionWeightings :: [Pitch] -> [Tension]
findMotionWeightings ps = zipWith (intervalTension .: interval) ps (drop 1 ps)

-- | Return the root of a chord.
-- >>> map getChordRoot $ collectPitchLists $ collectBeatLists $ breakAtEachEntrance bookExample
-- [64,64,71,64,69,62,64,57]
getChordRoot :: [[Pitch]] -> Pitch
getChordRoot =
    uncurry intervalRoot . strongestInterval . pairings' . nub . concat
  where
    strongestInterval = minimumBy (comparing (uncurry intervalStrength))
    -- This is only for compatibility with the original source code.
    -- Unison is the second weakest interval according to 'rootStrength' so it
    -- will usually not be the strongest of the chord.
    pairings' xs = let m = minimum xs
                   in (m, m) : pairings xs

{----------------------------------------------------------------------------
    Utility functions not used in the original Lisp code
-----------------------------------------------------------------------------}

-- | Return the object contained in a 'Marked'.
unmark :: Marked a -> a
unmark (Unstarred x) = x
unmark (Starred x) = x


-- | Helper function to convert from Cope's "event" type to 'Note'.
readNote :: (Time, Pitch, Time, Int, Int) -> Note
readNote (a, b, c, d, _) = Note { pitch = b
                                , start = a
                                , duration = c
                                , channel = d
                                }

-- | Return `True` if a 'Marked' object is 'Starred'.
isStarred :: Marked a -> Bool
isStarred (Starred _) = True
isStarred _           = False
