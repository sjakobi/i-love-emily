{-----------------------------------------------------------------------------
    SARA
------------------------------------------------------------------------------}
module SARA.Database where

import Data.List
import Data.List.Split  -- from the "split" package
import Data.Maybe
import qualified Data.Map as Map

import SARA.Types

type Map = Map.Map

{-----------------------------------------------------------------------------
    Creating a database
------------------------------------------------------------------------------}
data Database = Database
    { dbMeasures        :: Map Name Measure
    , dbLexicons        :: Map (Name,AnalysisLabel) Lexicon
    , dbCadenceLexicons :: Map Name CadenceLexicon
    } deriving (Show)

-- | Build a database from a list of pieces.
makeDatabase :: [Piece] -> Database
makeDatabase pieces = Database
    { dbMeasures = Map.fromList $ concatMap pMeasures pieces

    -- build analysis lexicon from measures
    , dbLexicons = Map.fromListWith unionLexicon $
        [ (key, lexicon)
        | piece <- pieces
        , (measureName, measure) <- pMeasures piece
        , not ("-cadence" `isSuffixOf` measureName)
        , let key     = (getSection measureName, head (analysis measure))
        , let lexicon = singletonLexicon (measureName, measure)
        ]

    , dbCadenceLexicons = Map.map buildCadenceLexicon $ Map.fromList $
        [ (key, lexicon)
        | piece <- pieces
        , let key     = getSection $ pName piece
        , let lexicon = pCadences  $ piece
        ]
    }

singletonLexicon :: (Name, Measure) -> Lexicon
singletonLexicon (name, measure) = Lexicon
    { functionList  = Map.fromList [(meter measure, [name])]
    , firstNoteList = []
    , lastChord     = ()
    }

unionLexicon :: Lexicon -> Lexicon -> Lexicon
unionLexicon a b = Lexicon
    { functionList  = Map.unionWith (++) (functionList a) (functionList b)
    , firstNoteList = firstNoteList a ++ firstNoteList b
    , lastChord     = ()
    }


data Piece = Piece
    { pMeasures :: [(Name, Measure)]
    , pCadences :: [(CadenceType, Name)]
    , pName     :: Name
    } deriving (Show)

data CadenceType = Full | Half
    deriving (Eq, Ord, Show)

buildCadenceLexicon :: [(CadenceType, Name)] -> CadenceLexicon
buildCadenceLexicon = foldl' f empty
    where
    f lexicon (Full,x) = lexicon { fullCadenceList = x : fullCadenceList lexicon }
    f lexicon (Half,x) = lexicon { halfCadenceList = x : halfCadenceList lexicon }
    empty = CadenceLexicon [] []

-- | Prepare a piece for inclusion in the database.
makePiece name (cadenceType, cadence) measures = Piece
    { pMeasures = (name ++ "-cadence", cadence)
                 : zip (map (makeMeasureName name) [1..]) measures
    , pName     = name
    , pCadences = [(cadenceType, name ++ "-cadence")]
    }

-- | Create names for measures of a piece.
makeMeasureName :: String -> Int -> Name
makeMeasureName name k = name ++ "-mea-" ++ show k

makeMeasureNames name n = map (makeMeasureName name) [1..n]


{-----------------------------------------------------------------------------
    Reading from the database
------------------------------------------------------------------------------}
-- | Replacement for  (eval measure-name)
evalMeasure :: Database -> Name -> Measure
evalMeasure db name = fromJust $ Map.lookup name $ dbMeasures db

-- | Replacement for  (eval (concat ... '-cadence))
evalCadence :: Database -> Name -> Measure
evalCadence db name =
    fromJust $ Map.lookup (name ++ "-cadence") $ dbMeasures db

-- | Replacement for  (eval (concat ... '-lexicon))
--
-- Returns a list of measures 
evalLexicon :: Database -> Name -> AnalysisLabel -> Lexicon
evalLexicon db name label =
    fromJust $ Map.lookup (name, label) $ dbLexicons db

-- | Replacement for  (eval (concat ... '-cadence-lexicon))
--
-- Returns a list of measures (that are cadences).
evalCadenceLexicon :: Database -> Name -> CadenceLexicon
evalCadenceLexicon db name =
    fromJust $ Map.lookup name $ dbCadenceLexicons db


-- | Given a measure, find the next measure of the original piece
-- in the database.
--
-- >>> nextMeasure "mozart-pf-pm-k283/2/1-mea-1"
-- "mozart-pf-pm-k283/2/1-mea-2"
nextMeasure :: Database -> Name -> Maybe Name
nextMeasure db measureName =
    findNext measureName $ measures (evalMeasure db measureName)
    where
    findNext x xs = listToMaybe $ drop 1 $ dropWhile (x /=) xs

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
-- | Extract composer, type, and section name.
--
-- >>> getSection "chopin-maz-one-7/5/5"
-- "chopin-maz-one"
getSection :: Name -> Name
getSection = intercalate "-" . take 3 . splitOn "-"

-- | Extract composer, type, section and phrase name.
--
-- >>> getPhrase "chopin-maz-one-7/5/5-mea-1"
-- "chopin-maz-one-7/5/5"
getPhrase :: Name -> Name
getPhrase = intercalate "-" . take 4 . splitOn "-"
