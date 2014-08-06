{-----------------------------------------------------------------------------
    Types
    for representing musical scores
------------------------------------------------------------------------------}
module Types where

type Time = Rational

data Note = Note
    { pitch    :: Int
    , start    :: Time
    , duration :: Time
    , channel  :: Int
    } deriving (Eq,Read,Show)

type Notes    = [Note]

type Metadata = [(String,String)]
type Score    = (Metadata, Notes)
