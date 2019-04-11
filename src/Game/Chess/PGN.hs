{-# LANGUAGE GADTs #-}
module Game.Chess.PGN (Outcome(..), PlyData(..), pgn) where

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Functor
import Data.Tree
import Data.Word
import Data.Void
import Game.Chess
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1 (L.skipLineComment ";") (L.skipBlockComment "{" "}")

lexeme = L.lexeme spaceConsumer

eog = lexeme $  string "1-0" $> Win White
            <|> string "0-1" $> Win Black
            <|> string "1/2-1/2" $> Draw
            <|> string "*" $> Undecided

sym = lexeme . fmap fst . match $ do
  void $ alphaNumChar
  many $ alphaNumChar <|> oneOf [35,43,45,58,61,95]

lbrace, rbrace, semi, period, quote, backslash :: Word8
lbrace    = fromIntegral $ ord '{'
rbrace    = fromIntegral $ ord '}'
semi      = fromIntegral $ ord ';'
period    = fromIntegral $ ord '.'
quote     = fromIntegral $ ord '"'
backslash = fromIntegral $ ord '\\'
lbracket  = void . lexeme . single . fromIntegral $ ord '['
rbracket  = void . lexeme . single . fromIntegral $ ord ']'
lparen    = void . lexeme . single . fromIntegral $ ord '('
rparen    = void . lexeme . single . fromIntegral $ ord ')'
nag = lexeme $ single (fromIntegral $ ord '$') *> L.decimal

comment :: Parser [Word8]
comment = single semi *> manyTill anySingle (eof <|> void eol)
      <|> single lbrace *> many (anySingleBut rbrace) <* single rbrace

tagPair = lexeme $ do
  lbracket
  k <- sym
  v <- str
  rbracket
  pure $ (k, v)

tagList = many tagPair

data Outcome = Win Color
             | Draw
             | Undecided
             deriving (Eq, Show)

data PlyData = PlyData {
  prefixNAG :: ![Int]
, ply :: !Move
, suffixNAG :: ![Int]
} deriving Show

movetext :: Position -> Parser (Outcome, Forest PlyData)
movetext pos = (,[]) <$> eog <|> main pos where
  main p = do
    pnags <- many nag
    validateMoveNumber p
    s <- sym
    snags <- many nag
    rav <- concat <$> many (lparen *> var p)
    case fromSAN p (BS.unpack s) of
      Right m ->
        (fmap . fmap) (\xs -> Node (PlyData pnags m snags) xs:rav) $
        movetext (unsafeApplyMove p m)
      Left e -> fail e
  var p = do
    pnags <- many nag
    validateMoveNumber p
    s <- sym
    snags <- many nag
    rav <- concat <$> many (lparen *> var p)
    case fromSAN p (BS.unpack s) of
      Right m ->
        fmap (\xs -> Node (PlyData pnags m snags) xs:rav) $
        rparen $> [] <|> var (unsafeApplyMove p m)
      Left e -> fail e
  validateMoveNumber p =
    optional (lexeme $ L.decimal <* space <* many (single period)) >>= \case
      Just n | moveNumber p /= n ->
        fail $ "Invalid move number: " <> show n <> " /= " <> show (moveNumber p)
      _ -> pure ()

pgn :: Parser [([(ByteString, String)], (Outcome, Forest PlyData))]
pgn = spaceConsumer *> many game <* spaceConsumer <* eof

game :: Parser ([(ByteString, String)], (Outcome, Forest PlyData))
game = do
  tl <- tagList
  pos <- case lookup "FEN" tl of
    Nothing -> pure startpos
    Just fen -> case fromFEN fen of
      Just p -> pure p
      Nothing -> fail "Invalid FEN"
  mt <- movetext pos
  pure $ (tl, mt)
  
str :: Parser String
str = p <?> "string" where
  p = (fmap . fmap) (chr . fromEnum) $ single quote *> many ch <* single quote
  ch = single backslash *> (  single backslash $> backslash
                          <|> single quote $> quote
                           )
   <|> anySingleBut quote
