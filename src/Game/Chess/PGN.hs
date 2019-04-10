module Game.Chess.PGN where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor
import Data.Tree
import Game.Chess

data Token = String ByteString
           | Integer Int
           | Period
           | NAG Int
           | Symbol ByteString
           deriving (Eq, Show)

whiteSpace = many $ void space <|> void endOfLine <|> void comment where
  comment = char ';' *> void (manyTill anyChar endOfLine)
         <|> char '{' *> many (notChar '}') *> void (char '}')

tagPair = do
  char '['
  whiteSpace
  k <- symbol
  whiteSpace
  v <- str
  whiteSpace
  char ']'
  pure $ (k, v)

tagList = sepBy tagPair whiteSpace

data GameTerminationMarker = Win Color
                           | Draw
                           | Undecided
                           deriving (Show)
gametermination = string "1-0" $> Win White
               <|> string "0-1" $> Win Black
               <|> string "1/2-1/2" $> Draw
               <|> char '*' $> Undecided

data PlyData = PlyData { prefixNAG :: [Int], ply :: Move, suffixNAG :: [Int] } deriving Show

mtext :: Position -> Parser (GameTerminationMarker, Forest PlyData)
mtext pos = (,[]) <$> endOfGame <|> main pos where
  main p = do
    pnags <- nag `sepBy'` whiteSpace
    whiteSpace
    validateMoveNumber p
    whiteSpace
    Symbol s <- symbol
    whiteSpace
    snags <- nag `sepBy'` whiteSpace
    whiteSpace
    rav <- concat <$> (char '(' *> whiteSpace *> var p) `sepBy'` whiteSpace
    whiteSpace
    case fromSAN p (BS.unpack s) of
      Right m -> fmap (\xs -> Node (PlyData pnags m snags) xs:rav) <$> mtext (unsafeApplyMove p m)
      Left e -> fail e
  var p = do
    pnags <- nag `sepBy'` whiteSpace
    whiteSpace
    validateMoveNumber p
    whiteSpace
    Symbol s <- symbol
    whiteSpace
    snags <- nag `sepBy'` whiteSpace
    whiteSpace
    rav <- concat <$> (char '(' *> whiteSpace *> var p) `sepBy'` whiteSpace
    whiteSpace
    case fromSAN p (BS.unpack s) of
      Right m ->
        (\xs -> Node (PlyData pnags m snags) xs:rav) <$> (char ')' $> [] <|> var (unsafeApplyMove p m))
      Left e -> fail e
  nag = char '$' *> decimal
  validateMoveNumber p =
    optional (decimal <* many space <* many (char '.')) >>= \case
      Just n | moveNumber p /= n ->
        fail $ "Invalid move number: " <> show n <> " /= " <> show (moveNumber p)
      _ -> pure ()

data Annotation = NumericAnn Int deriving Show


pgn = whiteSpace *> sepBy' game whiteSpace <* whiteSpace <* endOfInput
game = do
  tl <- tagList
  whiteSpace
  pos <- case lookup (Symbol "FEN") tl of
    Nothing -> pure startpos
    Just (String fen) -> case fromFEN (BS.unpack fen) of
      Just p -> pure p
      Nothing -> fail "Illegal FEN string"
  mt <- mtext pos
  pure $ (tl, mt)
  
endOfGame =
      char '*' $> Undecided
 <|> string "1/2-1/2" $> Draw
 <|> string "1-0" $> Win White
 <|> string "0-1" $> Win Black

str = fmap (String . BS.pack) $ "\"" *> many ch <* "\"" where
  ch = char '\\' *> (char '\\' $> '\\' <|> char '"' $> '"') <|> notChar '"'

symbol = fmap (Symbol . fst) . match $ do
  letter_ascii <|> digit
  many $ letter_ascii <|> digit <|> satisfy (`elem` ['_','+','#','=',':','-'])    
