{-----------------------------------------------------------------------------
    Write Score as a midi file
------------------------------------------------------------------------------}
module WriteMidi where

import Data.List
import Data.Function (on)

import Codec.Midi hiding (channel)

import Types
import ReadCope

-- | Export a score to midi format.
-- (We currently assume that time is measured in 1000 per beat.)
toMidi :: Score -> Midi
toMidi (_,notes) = Midi
        { fileType = MultiTrack
        , timeDiv  = timeDiv
        , tracks   = map toMessages $ unzipChannels notes
        }
    where
    velocity = 80
    timeDiv  = TicksPerBeat 1000
    
    unzipChannels notes = filter (not . null) $
        [filter ((ch ==) . channel) notes | ch <- [1..10]]

    toMessages :: Notes -> Track Ticks
    toMessages = fromAbsTime . sortBy (compare `on` fst) . concatMap toMessagePair
    
    toMessagePair :: Note -> Track Ticks
    toMessagePair n =
        [ (toTicks $ start n, NoteOn (channel n) (pitch n) velocity)
        , (toTicks $ start n + duration n, NoteOff (channel n) (pitch n) 0)
        ]
    
    toTicks x = floor x

-- | Example export
example = do
    x <- fmap readCope $ readFile "data/chopin-33-3"
    print $ toMidi x
    exportFile "chopin-33-3.mid" $ toMidi x
