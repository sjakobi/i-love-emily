{-----------------------------------------------------------------------------
	David Cope's Chorale program
------------------------------------------------------------------------------}
module Chorale where

import Types
import ReadCope

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set

import System.IO
import System.IO.Unsafe

example = unsafePerformIO $ fmap (snd . readCope) $ readFile "data/chopin-33-3.lisp"
example2 = drop 10 $ take 20 $ example

bachChoralesInDatabases :: [(String, Notes)]
bachChoralesInDatabases = []
	-- [("b206b", b206b), ...]

{-----------------------------------------------------------------------------
	Beat Database
------------------------------------------------------------------------------}
data BeatIt = BeatIt
	{ events           :: Notes -- ?
	, startNotes       :: [Pitch]
	, destinationNotes :: [Pitch]
	-- , startNote :: ??
	-- , startSet :: ??
	, voiceLeading :: ()
	-- , preDestinationNotes :: ??
	-- , texture :: ??
	, speac :: ()
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

	mkBeatIt beat next =
		BeatIt
			{ startNotes       = getOnsetNotes beat
			, destinationNotes = getOnsetNotes next
			, events           = beat
			, voiceLeading     = undefined
			, speac			   = ()
			}

removeNils   = id

-- | Decompose a piece of music into individual beats.
collectBeats :: Notes -> [Notes]
collectBeats notes = undefined
	where
	beat = collectByTiming notes
	collectByTiming = undefined

{-----------------------------------------------------------------------------
	Beat Utilities
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


-- | 
firstPlaceWhereAllTogether :: Notes -> Time
firstPlaceWhereAllTogether notes = allTogether orderedTimingsByChannel
	where
	endingTimes = plotTimings notes
	orderedTimingsByChannel = 
		[ collectTimingsByChannel endingTimes c
		| c <- getChannelNumbersFromEvents notes]
	

allTogether = undefined

-- | Get the channels and ending times of the notes.
plotTimings :: Notes -> [(Channel, Time)]
plotTimings xs = [(channel x, start x + duration x) | x <- xs]

-- | Collect the ending times by the indicated channel
collectTimingsByChannel :: [(Channel, Time)] -> Channel -> [Time]
collectTimingsByChannel xs c = [t | (c',t) <- xs, c == c' ] 



