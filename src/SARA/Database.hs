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
    Reading from the database
------------------------------------------------------------------------------}
data Database = Database
    { dbMeasures :: Map Name Measure
    , dbLexicons :: Map (Name,AnalysisLabel) [Name]
    } deriving (Show)

-- | Build a database from a list of pieces.
makeDatabase :: [Piece] -> Database
makeDatabase pieces = Database
    { dbMeasures = Map.fromList $ concatMap pMeasures pieces

    -- build analysis lexicon from measures
    , dbLexicons = Map.fromListWith (++) $
        [ ((getSection measureName, head (analysis measure)), [measureName])
        | piece <- pieces
        , (measureName, measure) <- pMeasures piece
        , not ("-cadence" `isSuffixOf` measureName)
        ]
    }

data Piece = Piece
    { pMeasures :: [(Name, Measure)]
    , pName     :: Name
    } deriving (Show)

-- | Prepare a piece for inclusion in the database.
makePiece name cadence measures = Piece
    { pMeasures = (name ++ "-cadence", cadence)
                 : zip (map (makeMeasureName name) [1..]) measures
    , pName     = name
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
evalLexicon :: Database -> Name -> AnalysisLabel -> [Name]
evalLexicon db name label =
    fromJust $ Map.lookup (name, label) $ dbLexicons db
    -- provisions for tiple, double, mono, etc measures ?


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
