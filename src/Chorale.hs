{-----------------------------------------------------------------------------
    David Cope's Chorale program
------------------------------------------------------------------------------}
module Chorale where

import           Data.Char         (ord)
import           Data.List         ( (\\), inits, intersperse
                                   , sort, sortBy, tails)
import           Data.Maybe
import           Data.Ord          (comparing)
import qualified Data.Set   as Set
import           Data.Set          (Set)
import qualified Data.Map   as Map
import           Data.Map          (Map)

import System.IO
import System.IO.Unsafe

import Types
import IO.ReadCope

{-----------------------------------------------------------------------------
    Examples
------------------------------------------------------------------------------}
example     = unsafePerformIO $ fmap (snd . readCope) $ readFile "data/chopin-33-3.lisp"
example2    = drop 10 $ take 20 $ example
exampleBach = [("b206b",b206b)]
    where
    b206b = unsafePerformIO $ fmap (readLispNotes . lines) $ readFile "data/b206b.lisp"

bachChoralesInDatabases :: [(String, Notes)]
bachChoralesInDatabases = []
    -- [("b206b", b206b), ...]

{-----------------------------------------------------------------------------
    Database
------------------------------------------------------------------------------}
data BeatIt = BeatIt
    { events           :: Notes
    , startNotes       :: [Pitch]
    , destinationNotes :: [Pitch]
    -- , startNote :: ??
    -- , startSet :: ??
    , voiceLeading     :: ([VoiceLeading], String, Time)
    -- , preDestinationNotes :: ??
    -- , texture :: ??
    , speac            :: ()
    -- , beat :: ??
    -- , lengthToCadence :: ??
    } deriving (Eq,Show,Read)


-- | Identifier for beats from the database.
type Name     = String
data Database = DB
    { composeBeats :: [Name]                 -- Every beat in the database.
    , startBeats   :: [Name]                 -- Beats at the beginnings of the pieces.
    , composeRules :: [([VoiceLeading],Name,Time)] -- Voice leading rules.
    , beatIts      :: Map Name BeatIt        -- Mapping from beat name to data.
    , lexicons     :: Map [Pitch] (Set Name) -- Beats that begin with these pitches.
    }

emptyDB :: Database
emptyDB = DB [] [] [] Map.empty Map.empty

makeName :: String -> Int -> String
makeName dbName counter = dbName ++ "-" ++ show counter

makeLexiconName :: String -> [Pitch] -> String
makeLexiconName name pitches = concat $ intersperse "-" $ name : map show pitches

-- | Create a complete database from a selection of pieces.
createCompleteDatabase :: [(String,Notes)] -> Database
createCompleteDatabase = foldl createBeatIts emptyDB

-- | Decompose a piece into individual beats
--   and decorate them with data about adjacent beats
createBeatIts :: Database -> (String,Notes) -> Database
createBeatIts db (dbName,notes) = db2
    where
    -- Question: What about the last beat in the measure?
    (db2,_,_) = foldl step (db,1,True) $ zip beats (drop 1 beats ++ [[]])

    beats = removeNils $ collectBeats $ setToZero $ sortByStart notes

    step (db, counter, isStart) (beat1, beat2) = (newdb, counter+1, False)
        where
        name  = makeName dbName counter

        newdb = db
            { composeBeats = name : composeBeats db
            , startBeats   = (if isStart then (name:) else id) (startBeats db)
            , composeRules = voiceLeading : composeRules db
            , beatIts      = Map.insert name beatit (beatIts db)
            , lexicons     = Map.alter putBeatIntoLexicon startNotes (lexicons db)
            }

        putBeatIntoLexicon Nothing    = Just (Set.singleton name)
        putBeatIntoLexicon (Just set)
            | name `Set.member` set   = Just set
            | otherwise               = Just (Set.insert name set)

        beatit = BeatIt
            { startNotes       = startNotes
            , destinationNotes = destinationNotes
            , events           = beat1
            , voiceLeading     = voiceLeading
            , speac            = ()
            }

        voiceLeading     = (getRules name startNotes destinationNotes
                           , name, start $ head $ sortByStart beat1)

        startNotes       = getOnsetNotes beat1
        destinationNotes = getOnsetNotes beat2

removeNils = filter (not . null)

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


-- | A rule for voice leading
data VoiceLeading = VL
    { dyad     :: Interval -- ^ Begins with a two-note chord
    , moveLow  :: Interval -- ^ The low note moves this way
    , moveHigh :: Interval -- ^ And the high note moves that way
    , nameVL   :: String   -- ^ Work name from which this was taken
                           -- (This field is to be removed later.)
    } deriving (Eq,Ord,Show,Read)


-- | Extract voice leading rules from two given chords.
--
-- >>> getRules [57,60,69,76] [59,62,67,79] "B206B-1"
-- [VL {dyad = 3, moveLow = 2, moveHigh = 2, nameVL = "B206B-1"},VL {dyad = 12, moveLow = 2, moveHigh = -2, nameVL = "B206B-1"},VL {dyad = 7, moveLow = 2, moveHigh = 3, nameVL = "B206B-1"},VL {dyad = 9, moveLow = 2, moveHigh = -2, nameVL = "B206B-1"},VL {dyad = 4, moveLow = 2, moveHigh = 3, nameVL = "B206B-1"},VL {dyad = 7, moveLow = -2, moveHigh = 3, nameVL = "B206B-1"}]
getRules :: String -> [Pitch] -> [Pitch] -> [VoiceLeading]
getRules name xs ys = map mkVoiceLeading $ pairings $ zip xs ys
    where
    mkVoiceLeading ((a,c),(b,d)) = VL
        { dyad     = reduceInterval (b - a)
        , moveLow  = c - a
        , moveHigh = d - b
        , nameVL   = name
        }

-- | Return all ways to choose two elements of the list.
-- In the result pairs, the first component always comes earlier in the list
-- than the second.
--
-- >>> pairings [1..4]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
pairings :: [a] -> [(a,a)]
pairings xs = [(y,z) | (y:ys) <- tails xs, z <- ys]


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

-- | Set of pitch classes in the given list of pitches.
--
-- A pitch class is the pitch modulo octaves. Middle C has pitch class 0.
createPitchClassSet :: [Pitch] -> Set Pitch
createPitchClassSet = Set.fromList . map (`mod` 12)

-- | Test whether a voicing forms a triad (major, minor, diminished, augmented),
-- in any inversion.
isTriad :: [Pitch] -> Bool
isTriad = any isRootTriad . inversions . Set.toAscList . createPitchClassSet
    where
    isRootTriad [x,y,z] = isThird (y-x) && isThird (z-y)
    isRootTriad _       = False

-- | List all distinct inversions of a chord,
-- obtained by successively transposing the root note up one octave.
--
-- >>> inversions [0,4,7]
-- [[0,4,7],[4,7,12],[7,12,16]]
inversions :: [Pitch] -> [[Pitch]]
inversions xs = init $ zipWith (\x y -> y ++ map (+12) x) (inits xs) (tails xs)

-- | Test whether an interval is a minor or major third.
isThird :: Interval -> Bool
isThird x = x == 3 || x == 4

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
getOnsetNotes xs = map pitch $ filter ((start (head xs) ==) . start) xs

-- | Collect all channel numbers that occur in the notes
getChannelNumbersFromEvents :: Notes -> [Int]
getChannelNumbersFromEvents = Set.toList . Set.fromList . map channel

-- | Return all Notes that are not played on the indicated channel.
getOtherChannels :: Channel -> Notes -> Notes
getOtherChannels c = filter ((/= c) . channel)

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


-- | Remove all notes that beign within a beat from the first note.
--
-- Note: This function adds up all durations and assumes that the
-- events are consecutive. In particular, it only makes sense when
-- applied to a single channel.
--
-- >>> removeFullBeat [note 77000 41 500 4, note 77500 43 500 4]
-- []
removeFullBeat :: Notes -> Notes
removeFullBeat xs = drop segment xs
    where
    segment = length $ takeWhile (< 1000) $ scanl (+) 0 $ map duration xs

-- | Take the last part of the note that lasts only a fraction of a beat.
--
-- >>> remainder $ note 76000 41 1500 4
-- [Note {pitch = 41, start = 77000 % 1, duration = 500 % 1, channel = 4}]
remainder :: Note -> [Note]
remainder note
        | 1000*beats == duration note = []
        | otherwise                   = [note
            { start    = start    note + 1000*beats
            , duration = duration note - 1000*beats
            }]
    where
    beats = fromIntegral $ floor $ duration note / 1000

{-----------------------------------------------------------------------------
    Composition
------------------------------------------------------------------------------}
-- | Increment beat number.
--
-- WARNING: This function increments only the last digit in the string.
-- I think this is a bug.
--
-- >>> incfBeat "b35300b-42"
-- "b35300b-3"
--
-- TODO: Beat names should probably be modeled as pairs @(String, Int)@.
incfBeat :: Name -> Name
incfBeat name = getDBName name ++ "-" ++ (show $ ord (last name) - ord '0' + 1)
    where
    getDBName = takeWhile (/= '-')

-- | Compose a chorale by stitching together beats from the database
-- and ensuring that the piece has a proper cadence.
composeBach :: Database -> Prob Notes
composeBach db = do
    mbeats <- composeMaybePiece db
    let
        Just names = mbeats
        notes = reTime $ concat $ map events
              $ catMaybes [Map.lookup name (beatIts db) | name <- names]
        lastNote = last notes

        continue
            = isNothing mbeats
            -- TODO: Include this condition later
            -- || end lastNote <  15000
            || end lastNote > 200000
            || not (waitForCadence notes)
            || checkForParallel notes

    if continue
        then composeBach db
        else return $ finish notes

    where
    finish = id
    {- TODO: add this later
    finish = cadenceCollapse . transposeToBachRange
           . fixUpBeat . ensureNecessaryCadences

    fixUpBeat notes =
        if checkMT $ getOnBeat $ start $ head notes
        then notes
        else delayForUpbeat notes
    -}

reTime = id
waitForCadence   _= True
checkForParallel _ = False

data Mood = Major | Minor
    deriving (Eq,Ord,Show,Read)

-- | Try to compose a complete piece by stitching together beats from the database.
-- May fail.
composeMaybePiece :: Database -> Prob (Maybe [Name])
composeMaybePiece db = do
    mbeat <- pickTriadBeginning db
    case mbeat of
        Nothing   -> return Nothing
        Just (name, mood) -> do
            let
                loop :: Int -> Name -> Prob (Maybe [Name])
                loop counter name
                    | null (destinationNotes beatit)     = return Nothing
                    | counter > 5 -- 36
                      && findEventsDuration notes > 1000
                      && (if mood == Minor
                            then matchTonicMinor notes
                            else matchBachTonic  notes)  = return $ Just []
                    | otherwise                          = do
                        case pickNextBeat db name of
                            Nothing     -> return Nothing
                            Just pname' -> do
                                name'   <- pname'
                                mresult <- loop (counter+1) name'
                                return $ fmap (name:) mresult

                    where
                    Just beatit = Map.lookup name (beatIts db)
                    notes       = events beatit

            return . fmap (++ [name]) =<< loop 0 name

matchTonicMinor _ = True
matchBachTonic  _ = True
findEventsDuration _ = 2000

-- | Pick a suitable next beat from the database.
pickNextBeat :: Database -> Name -> Maybe (Prob Name)
pickNextBeat db name = do
    beatit  <- Map.lookup name                      (beatIts  db)
    choices <- Map.lookup (destinationNotes beatit) (lexicons db)
    return $ case Set.toList choices of
        [x] -> return x
        xs  -> choose $ xs \\ [name, incfBeat name]
            -- write an email concerning  incfBeat

-- | Pick a triad beginning. May fail.
--
-- TODO: Fix this implementation to actually be random.
pickTriadBeginning :: Database -> Prob (Maybe (Name,Mood))
pickTriadBeginning db = return $ Just (name,mood)
    where
    name        = head (startBeats db)
    Just beatit = Map.lookup name (beatIts db)
    mood        = matchTonicMood $ take 4 $ events beatit

matchTonicMood :: Notes -> Mood
matchTonicMood _ = Major


