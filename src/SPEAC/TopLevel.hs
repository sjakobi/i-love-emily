module SPEAC.TopLevel where

import           SPEAC.Analysis (average, runSpeac)
import           SPEAC.Tensions (Tension, breakAtEachEntrance,
                                 runTheSPEACWeightings, unmark)
import           Types

type FormLabel = Char

-- | Top level for the form functions.
runTheProgram events meter = undefined
-- breakIntoPhrases
-- doSpeacOnPhrases
-- getSpeacMiddleGround
-- groupForm
-- getSpeacBackGround
  where
    -- form, phraseBeginnings and phraseLabels are just test values,
    -- do not correspond to actual definitions in top-level.lisp
    form = zip phraseBeginnings phraseLabels
    phraseBeginnings = map (start . unmark . head) $ breakAtEachEntrance events
    phraseLabels = cycle "ab"

breakIntoPhrases :: Notes -> [Time] -> [Notes]
breakIntoPhrases events []          = [events]
breakIntoPhrases events (t:timings) = firstEvents : laterEvents
  where
    -- TODO: * use partition
    --       * maybe make assumptions about order of events and use
    --         span/break instead
    firstEvents = getEventsTo t events
    laterEvents = breakIntoPhrases (getEventsFrom t events) timings

getEventsTo :: Time -> Notes -> Notes
getEventsTo time = filter (\x -> start x < time)

getEventsFrom :: Time -> Notes -> Notes
getEventsFrom time = filter (\x -> start x >= time)

doSpeacOnPhrases :: [Notes] -> Int -> [([SpeacLabel], Tension)]
doSpeacOnPhrases phrases meter = map (`doSpeacOnPhrase` meter) phrases

doSpeacOnPhrase :: Notes -> Int -> ([SpeacLabel], Tension)
doSpeacOnPhrase phrase meter =
    (runSpeac weights avg, avg)
  where
    weights = runTheSPEACWeightings phrase startBeatNumber beatLength meter
    startBeatNumber = getTheStartBeatNumber phrase meter
    beatLength = lispRound (getLength phrase / 1000)
    avg = average weights

getTheStartBeatNumber :: Notes -> Int -> Int
getTheStartBeatNumber events meter = 1 + (onbeat `mod` meter)
  where onbeat = lispRound $ start (head events) / 1000

-- | A naive implementation of the Common Lisp 'round' function.
--
-- This function regards only the one-input, one-output case of the
-- Common Lisp 'round' function.
--
-- Doc: http://jtra.cz/stuff/lisp/sclr/round.html
-- Discussion: https://groups.google.com/forum/#!topic/comp.lang.lisp/kOzJWK3c5uU
lispRound :: (RealFrac a, Integral b) => a -> b
lispRound n
  | abs (n - fromIntegral (round n)) == 0.5 = if even (floor n)
                                                then floor n
                                                else ceiling n
  | otherwise = round n

-- | Return the total duration length of a sorted list of notes.
getLength :: Notes -> Time
getLength events = end (last events) - start (head events)

getSpeacMiddleground :: [([SpeacLabel], Tension)] -> [[(FormLabel, Time)]] -> [([SpeacLabel], Tension)]
getSpeacMiddleground speacLists groupedForm =
    map (\phrase -> let test = map snd phrase
                        avg = average test
                    in (runSpeac test avg, avg)) groupedSpeacLists
  where groupedSpeacLists = groupSpeacLists speacLists groupedForm

groupSpeacLists :: [([SpeacLabel], Tension)] -> [[(FormLabel, Time)]] -> [[([SpeacLabel], Tension)]]
groupSpeacLists _          []                 = []
groupSpeacLists speacLists groupedForm@(f:fs) = a : groupSpeacLists b fs
  where (a, b) = splitAt (length f) speacLists

groupForm :: [(FormLabel, Time)] -> [[(FormLabel, Time)]]
groupForm = undefined
