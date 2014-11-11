module Internal.Utils where

import Data.List (tails, minimumBy, unfoldr)
import Data.Ord

-- | Return all ways to choose two elements of the list.
-- In the result pairs, the first component always comes earlier in the list
-- than the second.
--
-- >>> pairings [1..4 :: Int]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
pairings :: [a] -> [(a,a)]
pairings xs = [(y,z) | (y:ys) <- tails xs, z <- ys]

-- | Split a list into equal-sized chunks.
--
-- >>> chunk 4 [1..10 :: Int]
-- [[1,2,3,4],[5,6,7,8],[9,10]]
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = ys : chunk n zs
    where
    (ys,zs) = splitAt n xs

-- | Find the list element that is closest to the given element.
findClosest :: (Ord a, Num a) => a -> [a] -> a
findClosest x ys = snd $ minimumBy (comparing fst) [(abs (x-y), y) | y <- ys]

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

-- | Unfold a list by repeatedly applying a function to it until it is
-- empty.
-- >>> let f xs = let (hh, tl) = splitAt 2 xs in (sum hh, tl)
-- >>> unfoldListUntilEmpty f [1..10 :: Int]
-- [3,7,11,15,19]
unfoldListUntilEmpty :: ([a] -> (b, [a])) -> [a] -> [b]
unfoldListUntilEmpty f = unfoldr (\xs -> if null xs
                                            then Nothing
                                            else Just (f xs))

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
