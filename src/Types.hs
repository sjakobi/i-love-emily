{-----------------------------------------------------------------------------
    Types
    for representing musical scores
------------------------------------------------------------------------------}
module Types where

type Time    = Rational
type Pitch   = Int
type Channel = Int

data Note = Note
    { pitch    :: Pitch
    , start    :: Time
    , duration :: Time
    , channel  :: Channel
    } deriving (Eq,Read,Show)

type Notes    = [Note]

type Metadata = [(String,String)]
type Score    = (Metadata, Notes)
