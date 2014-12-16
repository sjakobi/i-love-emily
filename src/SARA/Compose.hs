{-----------------------------------------------------------------------------
    SARA
------------------------------------------------------------------------------}
module SARA.Compose


-- | This is the workhorse compose function.
simpleCompose :: Database -> Name -> Name -> Int -> Meter -> [???]
simpleCompose db name measureName number meter
    | number == 0 = []
    | otherwise   = result
    where
    -- *cadence-match*
    cadenceMatch =
        if  isMatch (evalMeasure db measureName)
            && number == 1
            && isNothing (nextMeasure measureName)
            && isMatch (evalCadence (getPhrase measureName))
        then Just $ getPhrase measureName
        else Nothing

    result
        = interchangeChannels measureName meter
        : simpleCompose name newMeasure (next number) meter

    next n = n - 1  -- simpleCompose counts down until it reaches 0.

    newMeasure
        -- return the original next measure if we want to match closely
        | isMatch (evalMeasure db measureName)
          && isJust (nextMeasure measureName)
          && isMatch (evalMeasure db (nextMeasure measureName)) =
            nextMeasure measureName

        -- choose a predominant
        | number == 2 = (let ((pre-dominant-list (get-predominant destinations)))
                          (make-best-choice
                           (get-destination-note measure-name)
                           pre-dominant-list
                           (get-new-first-notes-list last-chord pre-dominant-list))))

        -- make a chood choice
        | otherwise   = (make-best-choice
                           (get-destination-note measure-name)
                           (remove-matched-objects (remove-last-chord last-chord destinations))
                           (get-new-first-notes-list last-chord
                                (remove-matched-objects
                                (remove-last-chord last-chord destinations))))))

    destinations = getDestinations name measureName meter
    lastChord    = getLastChord name measureName

-- | Get measures name that have the same analysis label as the destination.
getDestinations :: Database -> Name -> Name -> Meter -> [Name]
getDestinations db name measureName meter =
    fromJust $ Map.lookup (functionList lexicon) meter
    where
    analysis = snd $ destination $ evalMeasure db measureName
    lexicon  = evalLexicon db name analysis
    -- If this lexicon doesn't exist, then pick from the network
    {-
      (let ((new-test (concat name '- (first *network*) '-lexicon)))
            (setq *network* (nthcdr *meter* *network*))
                       new-test))))))
    -}

spliceChannels       = undefined

-- | Return analysis and music, but with music from other
-- appropriate measures interleaved.
interchangeChannels :: Database -> Name -> Meter -> Prob ([AnalysisLabel],Notes)
interchangeChannels db measureName meter = do
    k <- makeRandom 100
    return (analysis measure,
        if recombinance > 60 && k < recombinance
        then spliceChannels db measureName meter
        else music measure)
    where
    measure = evalMeasure db measureName


-- | Randomly choose a pickup.
--
-- This function seems to be defunct.
-- None of the pieces in the database have an "incipient gesture".
chooseIncipientGesture :: Name -> [([String], [AnalysisLabel])]
chooseIncipientGesture _ = [(["incipience"], [])]
