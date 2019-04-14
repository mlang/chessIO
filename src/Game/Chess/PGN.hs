{-# LANGUAGE GADTs #-}
module Game.Chess.PGN where

import Control.Monad
import Data.Bifunctor
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Prettyprint.Doc hiding (space)
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Tree
import Data.Word
import Data.Void
import Game.Chess
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

newtype PGN = PGN [Game] deriving (Eq, Monoid, Semigroup)
type Game = ([(ByteString, Text)], (Outcome, Forest PlyData))
data Outcome = Win Color
             | Draw
             | Undecided
             deriving (Eq, Show)

data PlyData = PlyData {
  prefixNAG :: ![Int]
, ply :: !Move
, suffixNAG :: ![Int]
} deriving (Eq, Show)

readPGNFile :: FilePath -> IO (Either String PGN)
readPGNFile fp = first errorBundlePretty . parse pgn fp <$> BS.readFile fp

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

lbraceChar, rbraceChar, semiChar, periodChar, quoteChar, backslashChar, dollarChar :: Word8
lbraceChar    = fromIntegral $ ord '{'
rbraceChar    = fromIntegral $ ord '}'
semiChar      = fromIntegral $ ord ';'
periodChar    = fromIntegral $ ord '.'
quoteChar     = fromIntegral $ ord '"'
backslashChar = fromIntegral $ ord '\\'
dollarChar    = fromIntegral $ ord '$'
lbracketP = void . lexeme . single . fromIntegral $ ord '['
rbracketP = void . lexeme . single . fromIntegral $ ord ']'
lparenP   = void . lexeme . single . fromIntegral $ ord '('
rparenP   = void . lexeme . single . fromIntegral $ ord ')'
nag = lexeme $  single dollarChar *> L.decimal
            <|> string "!!" $> 3
            <|> string "??" $> 4
            <|> string "!?" $> 5
            <|> string "?!" $> 6
            <|> string "!"  $> 1
            <|> string "?"  $> 2

comment :: Parser String
comment = (fmap . fmap) (chr . fromEnum) $
      single semiChar *> manyTill anySingle (eof <|> void eol)
  <|> single lbraceChar *> many (anySingleBut rbraceChar) <* single rbraceChar

tagPair = lexeme $ do
  lbracketP
  k <- sym
  v <- str
  rbracketP
  pure $ (k, v)

tagList = many tagPair

movetext :: Position -> Parser (Outcome, Forest PlyData)
movetext pos = (,[]) <$> eog <|> main pos where
  main p = do
    pnags <- many nag
    validateMoveNumber p
    m <- lexeme $ relaxedSAN p
    snags <- many nag
    rav <- concat <$> many (lparenP *> var p)
    (fmap . fmap) (\xs -> Node (PlyData pnags m snags) xs:rav) $
      movetext (unsafeApplyMove p m)
  var p = do
    pnags <- many nag
    validateMoveNumber p
    m <- lexeme $ relaxedSAN p
    snags <- many nag
    rav <- concat <$> many (lparenP *> var p)
    fmap (\xs -> Node (PlyData pnags m snags) xs:rav) $
      rparenP $> [] <|> var (unsafeApplyMove p m)
  validateMoveNumber p =
    optional (lexeme $ L.decimal <* space <* many (single periodChar)) >>= \case
      Just n | moveNumber p /= n ->
        fail $ "Invalid move number: " <> show n <> " /= " <> show (moveNumber p)
      _ -> pure ()

pgn :: Parser PGN
pgn = spaceConsumer *> fmap PGN (many game) <* spaceConsumer <* eof

game :: Parser Game
game = do
  tl <- tagList
  pos <- case lookup "FEN" tl of
    Nothing -> pure startpos
    Just fen -> case fromFEN (T.unpack fen) of
      Just p -> pure p
      Nothing -> fail "Invalid FEN"
  mt <- movetext pos
  pure $ (tl, mt)
  
str :: Parser Text
str = p <?> "string" where
  p = fmap (T.pack . fmap (chr . fromEnum)) $ single quoteChar *> many ch <* single quoteChar
  ch = single backslashChar *> (  single backslashChar $> backslashChar
                          <|> single quoteChar $> quoteChar
                           )
   <|> anySingleBut quoteChar

type RAVOrder a = (Forest PlyData -> a) -> Forest PlyData -> [a]

breadthFirst, depthFirst :: RAVOrder a
breadthFirst _ [] = []
breadthFirst f ts = pure $ f ts
depthFirst f = fmap $ f . pure

pgnDoc :: RAVOrder (Doc ann) -> PGN -> Doc ann
pgnDoc ro (PGN games) = vsep $ fmap (gameDoc ro) games

gameDoc :: RAVOrder (Doc ann) -> Game -> Doc ann
gameDoc ro (tl, mt)
  | null tl = moveDoc ro pos mt
  | otherwise = tagsDoc tl <> line <> line <> moveDoc ro pos mt
 where
  pos | Just fen <- lookup "FEN" tl = fromJust $ fromFEN (T.unpack fen)
      | otherwise = startpos

tagsDoc :: [(ByteString, Text)] -> Doc ann
tagsDoc = vsep . fmap tagpair where
  tagpair (k, esc -> v) = brackets $ pretty (BS.unpack k) <+> dquotes (pretty v)
  esc = T.concatMap e where
    e '\\' = T.pack "\\\\"
    e '"' = T.pack "\\\""
    e c = T.singleton c


moveDoc :: RAVOrder (Doc ann) -> Position -> (Outcome, Forest PlyData) -> Doc ann
moveDoc ro pos (o,ts) = fillSep (go pos True ts <> [outcome o]) <> line where
  go _ _ [] = []
  go pos pmn (t:ts)
    | color pos == White || pmn
    = pnag <> (mn:san:snag) <> rav <> go pos' (not . null $ rav) (subForest t)
    | otherwise
    = pnag <> (san:snag) <> rav <> go pos' (not . null $ rav) (subForest t)
   where
    pl = ply . rootLabel $ t
    san = pretty (toSAN pos pl)
    pos' = unsafeApplyMove pos pl
    pnag = nag <$> prefixNAG (rootLabel t)
    mn = pretty (moveNumber pos) <> if color pos == White then "." else "..."
    rav = ro (parens . fillSep . go pos True) ts
    snag = nag <$> suffixNAG (rootLabel t)
  outcome (Win White) = "1-0"
  outcome (Win Black) = "0-1"
  outcome Draw        = "1/2-1/2"
  outcome Undecided   = "*"
  nag n = "$" <> pretty n
