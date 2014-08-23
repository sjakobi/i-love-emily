{-----------------------------------------------------------------------------
    David Cope's Chorale program
------------------------------------------------------------------------------}
module Chorale where

import Types
import ReadCope

import           Data.List         (sortBy, tails)
import           Data.Maybe
import           Data.Ord          (comparing)
import qualified Data.Set   as Set

import System.IO
import System.IO.Unsafe

example = unsafePerformIO $ fmap (snd . readCope) $ readFile "data/chopin-33-3.lisp"
example2 = drop 10 $ take 20 $ example
exampleBach =
    [ note 0 57 1000 4, note 0 60 1000 3, note 0 69 1000 2, note 0 76 1000 1
    , note 1000 59 1000 4, note 1000 62 1000 3, note 1000 67 1000 2, note 1000 79 1000 1
    , note 2000 60 500 4, note 2000 64 500 3, note 2000 67 500 2, note 2000 76 500 1
    , note 2500 59 500 4, note 2500 62 500 3, note 2500 68 500 2, note 2500 76 500 1
    , note 3000 57 500 4, note 3000 60 500 3, note 3000 69 500 2, note 3000 76 500 1
    , note 3500 56 500 4, note 3500 59 500 3, note 3500 71 500 2, note 3500 76 500 1
    ]
    where
    note a b c d = Note { pitch = b, start = a, duration = c, channel = d }

{-

Note 0 

((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96) (0 76 1000 1 96) (1000 59 1000 4 96)
 (1000 62 1000 3 96) (1000 67 1000 2 96) (1000 79 1000 1 96)

-}

bachChoralesInDatabases :: [(String, Notes)]
bachChoralesInDatabases = []
    -- [("b206b", b206b), ...]

{-----------------------------------------------------------------------------
    Database
------------------------------------------------------------------------------}
data BeatIt = BeatIt
    { events           :: Notes -- ?
    , startNotes       :: [Pitch]
    , destinationNotes :: [Pitch]
    -- , startNote :: ??
    -- , startSet :: ??
    , voiceLeading     :: ([Rule], String, Time)
    -- , preDestinationNotes :: ??
    -- , texture :: ??
    , speac            :: ()
    -- , beat :: ??
    -- , lengthToCadence :: ??
    }

createCompleteDatabase :: [(String,Notes)] -> [BeatIt]
createCompleteDatabase = concat . map createBeatIts

-- | Decompose a piece into individual beats
--   and decorate them with data about adjacent beats
createBeatIts :: (String,Notes) -> [BeatIt]
createBeatIts (_,notes) =
        zipWith mkBeatIt beats (drop 1 beats)
    where
    beats = removeNils $ collectBeats $ setToZero $ sortByStart notes
    name  = "bach"

    mkBeatIt beat next =
            BeatIt
                { startNotes       = startNotes
                , destinationNotes = destinationNotes
                , events           = beat
                , voiceLeading     =
                    (getRules startNotes destinationNotes name
                    , name, start $ head $ sortByStart beat)
                    -- also add these rules to the composer rules database
                , speac            = ()
                }
        where
        startNotes       = getOnsetNotes beat
        destinationNotes = getOnsetNotes next

removeNils   = id

-- | Decompose a piece of music into individual beats.
collectBeats :: Notes -> [Notes]
collectBeats []    = []
collectBeats notes = beat : collectBeats rest
    where
    beat = collectByTiming (firstPlaceWhereAllTogether notes) notes
    rest = drop (length beat) notes

-- | Return only those notes which end before the first argument
collectByTiming :: Time -> Notes -> Notes
collectByTiming time = filter ((<= time) . end)
    -- TODO: use a more clever data structure to turn this into a takeWhile


-- | Rule for voice leading.
data Rule = Rule Interval Interval Interval String
    deriving (Eq,Ord,Show,Read)

-- | Get intervals between adjacent sets of the two arguments.
--
-- >>> getRules [57,60,69,76] [59,62,67,79] "B206B-1"
-- [Rule 3 2 2 "B206B-1", Rule 12 2 (-2) "B206B-1", Rule 7 2 3 "B206B-1"
-- , Rule 9 2 (-2) "B206B-1", Rule 4 2 3 "B206B-1", Rule 7 (-2) 3 "B206B-1"]
getRules :: [Pitch] -> [Pitch] -> String -> [Rule]
getRules xs ys name =
    concat $ zipWith
        (\(x:xs) (d:ds) -> getRule d x xs ds)
        (tails1 xs) (tails1 ds)
    where
    tails1 = init . tails
    ds     = zipWith (-) ys xs
    
    getRule voice note =
        zipWith $ \x d -> Rule (reduceInterval $ x-note) voice d name

{-----------------------------------------------------------------------------
    Pitch utilities
------------------------------------------------------------------------------}
-- | Reduce intervals that go beyond an octave.
-- 
-- Note that the information whether the interval is an upwards or downards
-- motion is preserved. The result is an interval from -12 to 12.
reduceInterval :: Interval -> Interval
reduceInterval x
    | abs x <= 12 = x
    | x < 0       = reduceInterval (x+12)
    | otherwise   = reduceInterval (x-12)

{-----------------------------------------------------------------------------
    Note Utilities
------------------------------------------------------------------------------}
-- | Adjust starting times so that the first note starts at zero.
setToZero :: Notes -> Notes
setToZero xs = [ x { start = start x - diff } | x <- xs ] 
    where
    diff = start $ head xs

-- | Sort notes by starting times
sortByStart :: Notes -> Notes
sortByStart = sortBy (comparing start)

-- | Get the pitches of the notes that start simultaneously
-- with the first note.
getOnsetNotes :: Notes -> [Pitch]
getOnsetNotes (x:xs) = map pitch $ filter ((start x ==) . start) xs

-- | Collect all channel numbers that occur in the notes
getChannelNumbersFromEvents :: Notes -> [Int]
getChannelNumbersFromEvents = Set.toList . Set.fromList . map channel 

{-----------------------------------------------------------------------------
    Time utilities
------------------------------------------------------------------------------}
type Timing = (Channel, Time)

-- | "This looks ahead to get the first time they end together".
firstPlaceWhereAllTogether :: Notes -> Time
firstPlaceWhereAllTogether notes = allTogether orderedTimingsByChannel
    where
    endingTimes = plotTimings notes
    orderedTimingsByChannel = 
        [ collectTimingsByChannel endingTimes c
        | c <- getChannelNumbersFromEvents notes]

-- | "Returns the appropriate channel timing."
allTogether :: [[Timing]] -> Time
allTogether (c:cs)
    | c == []   = snd $ last $ last cs -- here is our remaining problem!!!!!
    | Just x <- findAlignmentInAllChannels (snd $ head c) cs = x
    | otherwise = allTogether (tail c:cs)

findAlignmentInAllChannels :: Time -> [[Timing]] -> Maybe Time
findAlignmentInAllChannels point channels
    | null channels = Just point
    | findAlignment point (head channels) =
        findAlignmentInAllChannels point (tail channels)
    | otherwise     = Nothing


-- | Checks whether a given time appears in the channel.
-- findAlignment 1000 [(4,1000),(4,1000),(4,5000)] == True
findAlignment :: Time -> [Timing] -> Bool
findAlignment point channel
    | null channel = False
    | thousandp point
      && isJust (lookup point $ map swap channel)
                   = True
    | otherwise    = findAlignment point (tail channel)

swap (x,y) = (y,x)

-- | Checks whether the number is a multiple of thousand.
thousandp :: Time -> Bool
thousandp n = 0 == (round n `mod` 1000)

-- | Get the channels and ending times of the notes.
plotTimings :: Notes -> [Timing]
plotTimings xs = [(channel x, end x) | x <- xs]

-- | Collect the ending times by the indicated channel
collectTimingsByChannel :: [Timing] -> Channel -> [Timing]
collectTimingsByChannel xs c = [x | x@(c',_) <- xs, c == c' ] 



