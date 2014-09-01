{-----------------------------------------------------------------------------
    Types
    for representing musical scores
------------------------------------------------------------------------------}
module Types where

import Control.Monad.Trans.State
import Data.List                 (sortBy)
import Data.Ord                  (comparing)
import System.Random

type Time     = Rational
type Pitch    = Int
type Interval = Pitch

-- | Determine the interval between two pitches.
interval :: Pitch -> Pitch -> Interval
interval a b = abs (a - b) `rem` 12

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

-- | Sort notes chronologically
sortByStart :: Notes -> Notes
sortByStart = sortBy (comparing start)


type Metadata = [(String,String)]
type Score    = (Metadata, Notes)

-- Monad for probabilistic computations.
type Prob = State StdGen

runProb :: Int -> Prob a -> [a]
runProb seed m = fst $ runState (sequence $ repeat m) (mkStdGen seed)

runProb1 :: Int -> Prob a -> a
runProb1 seed m = fst $ runState m (mkStdGen seed)

choose :: [a] -> Prob a
choose xs = do
    k <- state $ \s -> randomR (0,length xs-1) s
    return (xs !! k)
