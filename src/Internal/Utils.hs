module Internal.Utils where

import           Types

import           Data.List (sortBy, tails)
import           Data.Ord  (comparing)


-- | Determine the interval between two pitches.
interval :: Pitch -> Pitch -> Interval
interval a b = abs (a - b) `rem` 12

-- | Sort notes chronologically
sortByStart :: Notes -> Notes
sortByStart = sortBy (comparing start)

-- | Return all ways to choose two elements of the list.
-- In the result pairs, the first component always comes earlier in the list
-- than the second.
--
-- >>> pairings [1..4 :: Int]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
pairings :: [a] -> [(a,a)]
pairings xs = [(y,z) | (y:ys) <- tails xs, z <- ys]

-- | @'spanPlus' p xs@ returns a tuple where the first element is the
-- longest prefix of @xs@ that satisfies @p@ plus the first element of the
-- rest if there is any. The remaining elements make up the second element
-- of the tuple.
-- >>> spanPlus (== 'a') "aabb"
-- ("aab","b")
-- >>> spanPlus (== 'a') "aa"
-- ("aa","")
spanPlus :: (a -> Bool) -> [a] -> ([a], [a])
spanPlus p xs = case span p xs of
                  (as, b:bs) -> (as ++ [b], bs)
                  t -> t

-- | Similar to a map.
-- The difference is that in @'takeUntilEmpty' f xs@, @f@ is repeatedly
-- applied to the entire list @xs@ until @xs@ is empty.
-- >>> let f xs = let (hh, tl) = splitAt 2 xs in (sum hh, tl)
-- >>> takeUntilEmpty f [1..10 :: Int]
-- [3,7,11,15,19]
takeUntilEmpty :: ([a] -> (b, [a])) -> [a] -> [b]
takeUntilEmpty _ [] = []
takeUntilEmpty f xs = y : takeUntilEmpty f xs'
  where (y, xs') = f xs

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
