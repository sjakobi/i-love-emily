{-----------------------------------------------------------------------------
    SARA
------------------------------------------------------------------------------}
module SARA.Types where

import Types

data Mode  = Major | Minor  deriving (Show)
type Tempo = Int -- beats per minute
type Meter = Int -- 3 for 3/4, 4 for 4/4

-- | The 'Measure' is rather general.
-- It represents a measure of music, but also keeps information
-- about the piece that this measure comes from.
--
-- Later on, we may want to split this type according to its use cases,
-- but for translating the SARA source,
-- an "untyped" type is easier to work with.
type Name = String
data Measure = Measure
    { creator              :: String
    , matching_line_number :: Int
    , mode                 :: Mode
    , tempo                :: Tempo
    , meter                :: Meter
    , measures             :: [Name]    -- measures in this piece
    
    , music       :: Notes
    , analysis    :: [AnalysisLabel]
    , destination :: ([Pitch], AnalysisLabel)
    } deriving (Show)

-- | Labels for the SPEAC analysis (as used in SARA).
-- 
-- Common values include  @"a1"@, @"c1"@ and so on.
type AnalysisLabel = String

-- | The @match?@ predicate from the SARA source code.
-- It has to do with the pattern matching component,
-- which recognizes a composer's musical signature.
--
-- For the moment, we always return 'False',
-- which means that no measures from the originals are forced
-- into the newly composed piece.
isMatch :: Measure -> Bool
isMatch _ = False

defaultMeasure = Measure
    { creator              = ""
    , matching_line_number = 1
    , mode                 = Major
    , tempo                = 60
    , meter                = 4
    , measures             = []
    
    , music       = []
    , analysis    = []
    , destination = ([], "a1")
    }

