{-----------------------------------------------------------------------------
    Load MIDI files that have been converted to LISP format by David Cope.
    Very sloppy, but works.
------------------------------------------------------------------------------}
module IO.ReadCope where

import Control.Applicative
import Data.Char
import Text.ParserCombinators.ReadP

import Internal.Utils
import Types

-- | Read a score file with metadata at the top.
readCope :: String -> Score
readCope file = (metadata, score)
    where
    xs       = lines file
    metadata = [(init x, y) | [x,y] <- map words (take 6 xs)]
    score    = readLispNotes $ drop 7 $ xs

-- | Read a sequence of MIDI events, each formatted as a LISP list.
readLispNotes :: [String] -> Notes
readLispNotes = map readNote . chunk 5 . concat . map words

readNote :: [String] -> Note
readNote ['(':a,b,c,d,e] = Note
    { pitch    = read b
    , start    = fromIntegral $ read a
    , duration = fromIntegral $ read c
    , channel  = read d
    }

{-----------------------------------------------------------------------------
    Parser combinators
------------------------------------------------------------------------------}
readBach :: String -> [(String, Notes)]
readBach = fst . head . readP_to_S (manyTill parseBachPiece eof)

lexeme p  = skipComments *> p <* skipComments
symbol    = lexeme . string

skipComment = () <$ string "#|" <* manyTill get (string "|#")
skipComments = do
    s <- look
    case s of
        (';':_)             -> manyTill get (char '\n') >> skipComments
        ('#':_)             -> skipComment >> skipComments
        (c  :_) | isSpace c -> skipSpaces  >> skipComments
        _                   -> return ()

parseBachPiece :: ReadP (String, Notes)
parseBachPiece = do
    symbol "("
    symbol "setq"
    name  <- lexeme $ munch1 isAlphaNum
    symbol "'("
    notes <- manyTill parseNote $ symbol ")"
    symbol ")"
    return (name, notes)

parseNote :: ReadP Note
parseNote =
    symbol "(" *> (makeNote <$> int <*> int <*> int <*> int <*> int) <* symbol ")"    
    where
    int = lexeme $ fmap read $ munch1 isDigit
    makeNote a b c d _ = Note
        { pitch    = b
        , start    = fromIntegral $ a
        , duration = fromIntegral $ c
        , channel  = d
        }
