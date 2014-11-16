{-----------------------------------------------------------------------------
    SARA
------------------------------------------------------------------------------}
module SARA.Database where

import Data.List
import Data.List.Split  -- from the "split" package

import SARA.Types

-- | Extract composer, type, and section name.
--
-- >>> getSection "chopin-maz-one-7/5/5"
-- "chopin-maz-one"
getSection :: Name -> Name
getSection = intercalate "-" . take 3 . splitOn "-"


-- | Replacement for  (eval (concat ... '-lexicon))
--
-- Returns a list of measures 
evalLexicon :: Database -> Name -> AnalysisLabel -> [Name]
evalLexicon db name label = 
    Map.lookup (name, label) (lexicon db)

    -- provisions for tiple, double, mono, etc measures ?

