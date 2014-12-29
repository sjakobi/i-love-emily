{-----------------------------------------------------------------------------
    SARA
------------------------------------------------------------------------------}
module SARA.Compose where

import           Data.List         ((\\))
import qualified Data.Map   as Map
import           Data.Maybe
import qualified Data.Set   as Set
import           Data.Set          (Set)

import SARA.Database
import SARA.Types
import Types

-- | Global setting that indicates how much the composition
-- algorithm should recombine existing material.
recombinance = 80

{-----------------------------------------------------------------------------
    Composition
------------------------------------------------------------------------------}
type Form = (String, Int)

dorepeat = False

-- | Compose a piece with both incipience and cadence.
compose :: Database -> Name -> Int -> Meter -> Form -> Prob [([AnalysisLabel], Notes)]
compose db creator numberOfMeasures meter form
    | recombinance < 10             = finiteStateTransition db meter
    | otherwise                     = do
        chooseIncipientGesture creator -- essentially a no-op
        start   <- chooseInitialChord creator meter
        (piece0, cadenceMatch)
                <- simpleCompose db creator start numberOfMeasures meter
        let piece = (if dorepeat then makeRepeat else id) piece0

        cadence <- case cadenceMatch of
            Just name    -> return $ music $ evalCadence db name
            Nothing      -> do
                k <- makeRandom 100
                if recombinance > 60 && k < recombinance
                    then spliceCadenceChannels
                            (chooseACadence db creator form)
                            (listAppropriateCadences $ snd form)
                    else do
                        name <- chooseACadence db creator form
                        return $ music $ evalCadence db name
        return $
            piece0 ++ [(["cadence"], cadence)]

makeRepeat              = undefined
chooseInitialChord      = undefined
finiteStateTransition   = undefined -- composition with a finite state machine

-- | This is the workhorse compose function.
simpleCompose
    :: Database -> Name -> Name -> Int -> Meter
    -> Prob ([([AnalysisLabel], Notes)] -- music with analysis labels
            , Maybe Name)               -- possible matching cadence
simpleCompose db creator measureName number meter
    | number == 0 = return ([], Nothing)
    | otherwise   = do
        x      <- interchangeChannels db measureName meter
        mnew   <- newMeasure
        (xs,c) <- case mnew of
            Just new -> simpleCompose db creator new (next number) meter
            Nothing  -> return ([], Nothing)

        if number == 1
            then return (x:xs, cadenceMatch)
            else return (x:xs, c)
    where
    cadenceMatch =
        if  isMatch (evalMeasure db measureName)
            && number == 1
            && isNothing (nextMeasure db measureName)
            && isMatch (evalCadence db (getPhrase measureName))
        then Just $ getPhrase measureName
        else Nothing

    next n = n - 1  -- simpleCompose counts down until it reaches 0.

    -- Choose the next measure.
    newMeasure
        -- return the original next measure if we want to match closely
        | isMatch (evalMeasure db measureName),
          Just next <- nextMeasure db measureName,
          isMatch (evalMeasure db next) =
            return $ Just next

        -- end prematurely if we cannot find a new measure to continue
        | null list = return Nothing
        -- make a new choice
        | otherwise = fmap Just $ makeBestChoice (getDestinationNote measureName)
                        list (getNewFirstNotesList db lastChord list)
        where
        list
            | number == 2 = getPredominant db destinations -- choose predominant
            | otherwise   = removeMatchedObjects db (removeLastChord lastChord destinations)

    getDestinationNote = head . fst . destination . evalMeasure db

    destinations = getDestinations db creator measureName meter
    lastChord    = getLastChord db creator measureName
    getLastChord _ _ = id
    -- Note: In the original SARA source code,
    -- the  get-last-chord  function has the side effect of setting
    -- a field in the lexicon entry.
    -- However, it appears that this field is never accessed,
    -- so we can skip this step.

-- | Remove all measure names that have been matched
-- by the matching component (which we don't use here).
removeMatchedObjects :: Database -> [Name] -> [Name]
removeMatchedObjects db = filter (not . isMatched . evalMeasure db)
    where
    isMatched measure = case getMatch measure of
        Just measure' -> matching_line_number measure' > 1
        Nothing       -> False

-- | Get the first note of each measure.
getNewFirstNotesList :: Database -> Name -> [Name] -> [Pitch]
getNewFirstNotesList db x xs
    = map (pitch . head . getSoundingChannel 1 . music . evalMeasure db)
    $ removeLastChord x xs

-- | Get measures name that have the same analysis label as the destination.
getDestinations :: Database -> Name -> Name -> Meter -> [Name]
getDestinations db name measureName meter =
    fromJust $ Map.lookup meter (functionList lexicon)
    where
    analysis = snd $ destination $ evalMeasure db measureName
    lexicon  = evalLexicon db name analysis
    -- If this lexicon doesn't exist, then pick from the network
    {-
      (let ((new-test (concat name '- (first *network*) '-lexicon)))
            (setq *network* (nthcdr *meter* *network*))
                       new-test))))))
    -}

-- | Return all measures that preview a cadence,
-- or the whole list if it contains only one measure.
getPredominant :: Database -> [Name] -> [Name]
getPredominant db [x] = [x]
getPredominant db xs  = filter (isPredominant . evalMeasure db) xs
    where
    isPredominant = (`elem` ["p2","c1","p1"]) . snd . destination


-- | Return a measure name that has the closes starting pitch.
makeBestChoice :: Pitch -> [Name] -> [Pitch] -> Prob Name
makeBestChoice pitch xs ys = findClosest pitch $ zip xs ys

findClosest :: Pitch -> [(a,Pitch)] -> Prob a
findClosest x ys = choose $ map fst $ filter ((dist ==) . distance) ys
    where
    distance (_,y) = abs $ x - y
    dist           = minimum $ map distance ys

-- | Remove an element from the list, but only if it is not the only element.
removeLastChord :: Eq a => a -> [a] -> [a]
removeLastChord _ [x] = [x]
removeLastChord x xs  = xs \\ [x]

-- | Interweave channels from measures with the same analysis label
spliceChannels :: Database -> Name -> Meter -> Prob Notes
spliceChannels db name meter
    | null candidates = return music1
    | otherwise       = do
        music2 <- choose candidates
        return $
            getSoundingChannel 1 music2
            ++ (music1 \\ getSoundingChannel 1 music1)
    where
    measure  = evalMeasure db name
    music1   = music measure
    label    = head $ analysis measure
    measureNames
        = maybe [] id
        $ Map.lookup meter
        $ functionList
        $ evalLexicon db (getSection name) label

    candidates = getMatchingAnalysisMusic db (analysis measure) measureNames

-- | Get music of all those measures whose (complete) analysis matches.
getMatchingAnalysisMusic :: Database -> [AnalysisLabel] -> [Name] -> [Notes]
getMatchingAnalysisMusic db as =
    map music . filter ((as ==) . analysis) . map (evalMeasure db)


-- | Return analysis and music, but with music from other
-- appropriate measures interleaved.
interchangeChannels :: Database -> Name -> Meter -> Prob ([AnalysisLabel],Notes)
interchangeChannels db measureName meter = do
    k     <- makeRandom 100
    notes <- if recombinance > 60 && k < recombinance
        then spliceChannels db measureName meter
        else return $ music measure
    return (analysis measure, notes)
    where
    measure = evalMeasure db measureName


{-----------------------------------------------------------------------------
    Incipient gestures
------------------------------------------------------------------------------}
-- | Randomly choose a pickup.
--
-- This function seems to be defunct.
-- None of the pieces in the database have an "incipient gesture".
chooseIncipientGesture :: Name -> Prob [([String], [AnalysisLabel])]
chooseIncipientGesture _ = return [(["incipience"], [])]

{-----------------------------------------------------------------------------
    Cadences
------------------------------------------------------------------------------}
spliceCadenceChannels = undefined
listAppropriateCadences = undefined

-- | Return a best cadence for the composition.
chooseACadence :: Database -> Name -> Form -> Prob Name
chooseACadence db creator (_,cadenceType) = chooseTheBestCadence $
    if cadenceType == 2 then ifnotnull full half else ifnotnull half full
    where
    full = fullCadenceList $ evalCadenceLexicon db creator
    half = halfCadenceList $ evalCadenceLexicon db creator
    ifnotnull xs ys = if null xs then ys else xs

chooseTheBestCadence = undefined
evalCadenceLexicon = undefined
fullCadenceList = undefined
halfCadenceList = undefined

{-----------------------------------------------------------------------------
    Note Utilities
------------------------------------------------------------------------------}
-- | Of the channels that are used, get all notes in the nth one.
-- (Counting begins at @1@).
getSoundingChannel :: Int -> Notes -> Notes
getSoundingChannel n notes = filter ((== c) . channel) notes
    where
    channels = getChannelNumbersFromEvents notes
    c        = channels !! (n-1)

-- | Collect all channel numbers that occur in the notes
getChannelNumbersFromEvents :: Notes -> [Int]
getChannelNumbersFromEvents = Set.toList . Set.fromList . map channel
