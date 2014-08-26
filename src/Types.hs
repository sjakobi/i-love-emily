{-----------------------------------------------------------------------------
    Types
    for representing musical scores
------------------------------------------------------------------------------}
module Types where

type Time     = Rational
type Pitch    = Int
type Interval = Pitch
type Channel  = Int

data Note = Note
    { pitch    :: Pitch
    , start    :: Time
    , duration :: Time
    , channel  :: Channel
    } deriving (Eq,Read,Show)

-- | Helper function for building notes from Lisp notation.
note :: Time -> Pitch -> Time -> Channel -> Note
note a b c d = Note { pitch = b, start = a, duration = c, channel = d }

end :: Note -> Time
end x = start x + duration x

type Notes    = [Note]

type Metadata = [(String,String)]
type Score    = (Metadata, Notes)
