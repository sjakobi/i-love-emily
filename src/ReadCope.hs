{-----------------------------------------------------------------------------
    Load MIDI files that have been converted to LISP format by David Cope.
    Very sloppy, but works.
------------------------------------------------------------------------------}
module ReadCope where

import Types

readCope :: String -> Score
readCope file = (metadata, score)
    where
    xs       = lines file
    metadata = [(init x, y) | [x,y] <- map words (take 6 xs)]
    score    = map readNote $ chunk 5 $ concat $ map words $ drop 7 $ xs

readNote :: [String] -> Note
readNote ['(':a,b,c,d,e] = Note
    { pitch    = read b
    , start    = fromIntegral $ read a
    , duration = fromIntegral $ read c
    , channel  = read d
    }


chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
    where
    (ys,zs) = splitAt n xs
