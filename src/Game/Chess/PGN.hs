{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-|
Module      : Game.Chess.PGN
Description : Portable Game Notation
Copyright   : (c) Mario Lang, 2021
License     : BSD3
Maintainer  : mlang@blind.guru
Stability   : experimental

A PGN file consists of a list of games.
Each game consists of a tag list, the outcome, and a forest of rosetrees.
-}
module Game.Chess.PGN (
  PGN(..)
, Game(..), cgTags, cgOutcome, cgForest
, Outcome(..), _Win, _Draw, _Undecided
, Annotated(..), annPrefixNAG, annPly, annSuffixNAG
, readPGNFile, gameFromForest, pgnForest
  -- * A PGN parser
, pgn
  -- * Prettyprinting
, hPutPGN, pgnDoc, RAVOrder, breadthFirst, depthFirst, gameDoc
, weightedForest
) where

import           Control.Lens                          (makeLenses, makePrisms,
                                                        makeWrapped)
import           Control.Monad                         (void)
import           Control.Monad.IO.Class                (MonadIO (..))
import           Data.Bifunctor                        (Bifunctor (first))
import           Data.ByteString.Char8                 (ByteString)
import qualified Data.ByteString.Char8                 as BS
import           Data.Char                             (chr, ord)
import           Data.Foldable                         (for_)
import           Data.Functor                          (($>))
import           Data.Hashable                         (Hashable (..))
import           Data.List                             (partition, sortOn)
import           Data.Maybe                            (fromJust, isNothing)
import           Data.Ord                              (Down (Down))
import           Data.Ratio                            ((%))
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    as T (decodeUtf8)
import           Data.Text.Prettyprint.Doc             (Doc,
                                                        FusionDepth (Shallow),
                                                        Pretty (pretty),
                                                        brackets, dquotes,
                                                        fillSep, fuse, line,
                                                        parens, vsep, (<+>))
import           Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import           Data.Tree                             (Tree (..), foldTree)
import           Data.Void                             (Void)
import           Data.Word                             (Word8)
import           GHC.Generics                          (Generic)
import           Game.Chess.Internal                   (Color (..), Ply,
                                                        Position (color, moveNumber),
                                                        fromFEN, startpos,
                                                        unsafeDoPly)
import           Game.Chess.SAN                        (relaxedSAN, unsafeToSAN)
import           Language.Haskell.TH.Syntax            (Lift)
import           System.IO                             (Handle, hPutStrLn)
import           Text.Megaparsec                       (MonadParsec (eof),
                                                        Parsec, anySingleBut,
                                                        errorBundlePretty, many,
                                                        match, oneOf, optional,
                                                        parse, single, (<?>),
                                                        (<|>))
import           Text.Megaparsec.Byte                  (alphaNumChar, space,
                                                        space1, string)
import qualified Text.Megaparsec.Byte.Lexer            as L

data Annotated a = Ann {
  _annPrefixNAG :: ![Int]
, _annPly       :: !a
, _annSuffixNAG :: ![Int]
} deriving (Eq, Functor, Generic, Lift, Show)

instance Applicative Annotated where
  pure a = Ann [] a []
  Ann pn1 f sn1 <*> Ann pn2 a sn2 = Ann (pn1 <> pn2) (f a) (sn1 <> sn2)

makeLenses ''Annotated

instance Hashable a => Hashable (Annotated a)

data Outcome = Win Color
             | Draw
             | Undecided
             deriving (Eq, Generic, Lift, Show)

instance Hashable Outcome

makePrisms ''Outcome

data Game = CG {
  _cgTags    :: ![(Text, Text)]
, _cgForest  :: ![Tree (Annotated Ply)]
, _cgOutcome :: !Outcome
} deriving (Eq, Generic, Show)

instance Hashable Game where
  hashWithSalt s CG { .. } = s
    `hashWithSalt` _cgTags
    `hashWithSalt` foldTree (hashWithSalt . hash) <$> _cgForest
    `hashWithSalt` _cgOutcome

makeLenses ''Game

newtype PGN = PGN [Game] deriving (Eq, Monoid, Semigroup)

makeWrapped ''PGN

gameFromForest :: [(Text, Text)] -> [Tree Ply] -> Outcome -> Game
gameFromForest tags forest _cgOutcome = CG { .. } where
  _cgTags = ("Result", r):tags
  _cgForest = (fmap . fmap) pure forest
  r = case _cgOutcome of
    Win White -> "1-0"
    Win Black -> "0-1"
    Draw      -> "1/2-1/2"
    Undecided -> "*"

pgnForest :: PGN -> [Tree Ply]
pgnForest (PGN gs) = merge $ concatMap ((fmap . fmap) _annPly . _cgForest) gs where
  merge :: Eq a => [Tree a] -> [Tree a]
  merge = foldl mergeTree [] where
    merge' l r = l { subForest = foldl mergeTree (subForest l) (subForest r) }
    mergeTree [] y = [y]
    mergeTree (x:xs) y
      | rootLabel x == rootLabel y = x `merge'` y : xs
      | otherwise = x : xs `mergeTree` y

instance Ord Outcome where
  Win _ `compare` Win _         = EQ
  Win _ `compare` _             = GT
  _ `compare` Win _             = LT
  Draw `compare` Draw           = EQ
  Draw `compare` _              = GT
  _ `compare` Draw              = LT
  Undecided `compare` Undecided = EQ

instance Pretty Outcome where
  pretty (Win White) = "1-0"
  pretty (Win Black) = "0-1"
  pretty Draw        = "1/2-1/2"
  pretty Undecided   = "*"

readPGNFile :: MonadIO m => FilePath -> m (Either String PGN)
readPGNFile fp = liftIO $
  first errorBundlePretty . parse pgn fp . stripBOM <$> BS.readFile fp

bom :: ByteString
bom = "\xEF\xBB\xBF"

stripBOM :: ByteString -> ByteString
stripBOM bs
  | bom `BS.isPrefixOf` bs = BS.drop (BS.length bom) bs
  | otherwise              = bs

hPutPGN :: Handle -> RAVOrder (Doc ann) -> PGN -> IO ()
hPutPGN h ro (PGN games) = for_ games $ \g -> do
  hPutDoc h $ gameDoc ro g
  hPutStrLn h ""

type Parser = Parsec Void ByteString

spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1 (L.skipLineComment ";") (L.skipBlockComment "{" "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

eog :: Parser Outcome
eog = lexeme $  string "1-0" $> Win White
            <|> string "0-1" $> Win Black
            <|> string "1/2-1/2" $> Draw
            <|> string "*" $> Undecided

sym :: Parser Text
sym = lexeme . fmap (T.decodeUtf8 . fst) . match $ do
  void alphaNumChar
  many $ alphaNumChar <|> oneOf [35,43,45,58,61,95]

periodChar, quoteChar, backslashChar, dollarChar :: Word8
periodChar    = fromIntegral $ ord '.'
quoteChar     = fromIntegral $ ord '"'
backslashChar = fromIntegral $ ord '\\'
dollarChar    = fromIntegral $ ord '$'

lbracketP, rbracketP, lparenP, rparenP :: Parser ()
lbracketP = void . lexeme . single . fromIntegral $ ord '['
rbracketP = void . lexeme . single . fromIntegral $ ord ']'
lparenP   = void . lexeme . single . fromIntegral $ ord '('
rparenP   = void . lexeme . single . fromIntegral $ ord ')'

nag :: Parser Int
nag = lexeme $  single dollarChar *> L.decimal
            <|> string "!!" $> 3
            <|> string "??" $> 4
            <|> string "!?" $> 5
            <|> string "?!" $> 6
            <|> string "!"  $> 1
            <|> string "?"  $> 2

tagPair :: Parser (Text, Text)
tagPair = lexeme $ do
  lbracketP
  k <- sym
  v <- str
  rbracketP
  pure (k, v)

tagList :: Parser [(Text, Text)]
tagList = many tagPair

movetext :: Position -> Parser (Outcome, [Tree (Annotated Ply)])
movetext pos = (,[]) <$> eog <|> main pos where
  main p = ply p >>= \(m, n) -> fmap n <$> movetext (unsafeDoPly p m)
  var p = ply p >>= \(m, n) -> n <$> (rparenP $> [] <|> var (unsafeDoPly p m))
  ply p = do
    pnags <- many nag
    validateMoveNumber p
    m <- lexeme $ relaxedSAN p
    snags <- many nag
    rav <- concat <$> many (lparenP *> var p)
    pure (m, \xs -> Node (Ann pnags m snags) xs:rav)
  validateMoveNumber p =
    optional (lexeme $ L.decimal <* space <* many (single periodChar)) >>= \case
      Just n | moveNumber p /= n ->
        fail $ "Invalid move number: " <> show n <> " /= " <> show (moveNumber p)
      _ -> pure ()

pgn :: Parser PGN
pgn = spaceConsumer *> fmap PGN (many game) <* spaceConsumer <* eof

game :: Parser Game
game = do
  _cgTags <- tagList
  pos <- case lookup "FEN" _cgTags of
    Nothing -> pure startpos
    Just fen -> case fromFEN (T.unpack fen) of
      Just p  -> pure p
      Nothing -> fail "Invalid FEN"
  (_cgOutcome, _cgForest) <- movetext pos
  pure $ CG { .. }

str :: Parser Text
str = p <?> "string" where
  p = fmap (T.pack . fmap (chr . fromEnum)) $ single quoteChar *> many ch <* single quoteChar
  ch = single backslashChar *> (  single backslashChar $> backslashChar
                              <|> single quoteChar $> quoteChar
                               )
    <|> anySingleBut quoteChar

type RAVOrder a = ([Tree (Annotated Ply)] -> a) -> [Tree (Annotated Ply)] -> [a]

breadthFirst, depthFirst :: RAVOrder a
breadthFirst _ [] = []
breadthFirst f ts = pure $ f ts
depthFirst f = fmap $ f . pure

pgnDoc :: RAVOrder (Doc ann) -> PGN -> Doc ann
pgnDoc ro (PGN games) = vsep $ gameDoc ro <$> games

gameDoc :: RAVOrder (Doc ann) -> Game -> Doc ann
gameDoc ro CG { .. }
  | null _cgTags = moveDoc ro pos (_cgOutcome, _cgForest)
  | otherwise = tagsDoc _cgTags <> line <> line <> moveDoc ro pos (_cgOutcome, _cgForest)
 where
  pos | Just fen <- lookup "FEN" _cgTags = fromJust $ fromFEN (T.unpack fen)
      | otherwise = startpos

tagsDoc :: [(Text, Text)] -> Doc ann
tagsDoc = fuse Shallow . vsep . fmap tagpair where
  tagpair (k, esc -> v) = brackets $ pretty k <+> dquotes (pretty v)
  esc = T.concatMap e where
    e '\\' = T.pack "\\\\"
    e '"'  = T.pack "\\\""
    e c    = T.singleton c

moveDoc :: RAVOrder (Doc ann) -> Position -> (Outcome, [Tree (Annotated Ply)])
        -> Doc ann
moveDoc ro p (o,f) = fillSep (go p True f <> [pretty o]) <> line where
  go _ _ [] = []
  go pos pmn (t:ts)
    | color pos == White || pmn
    = pnag <> (mn:san:snag) <> rav <> go pos' (not . null $ rav) (subForest t)
    | otherwise
    = pnag <> (san:snag) <> rav <> go pos' (not . null $ rav) (subForest t)
   where
    pl = _annPly . rootLabel $ t
    san = pretty $ unsafeToSAN pos pl
    pos' = unsafeDoPly pos pl
    pnag = prettynag <$> _annPrefixNAG (rootLabel t)
    mn = pretty (moveNumber pos) <> if color pos == White then "." else "..."
    rav = ro (parens . fillSep . go pos True) ts
    snag = prettynag <$> _annSuffixNAG (rootLabel t)
  prettynag n = "$" <> pretty n

weightedForest :: PGN -> [Tree (Rational, Ply)]
weightedForest (PGN games) = merge . concatMap rate $ filter ok games where
  ok CG { .. } = isNothing (lookup "FEN" _cgTags) && _cgOutcome /= Undecided
  rate CG { .. } = f startpos <$> trunk _cgForest where
    w c = case _cgOutcome of
      Win c' | c == c' -> 1
             | otherwise -> -1
      Draw -> 1 % 2
      Undecided -> 0
    f pos (Node a ts') = Node (w (color pos), _annPly a) $
      f (unsafeDoPly pos (_annPly a)) <$> ts'
  trunk []    = []
  trunk (x:_) = [x { subForest = trunk (subForest x)}]
  merge [] = []
  merge ((Node a ts) : xs) =
      sortOn (Down . fst . rootLabel)
    $ Node (w, snd a) (merge $ ts ++ concatMap subForest good) : merge bad
   where
    (good, bad) = partition (eq a . rootLabel) xs where eq x y = snd x == snd y
    w = fst a + sum (fst . rootLabel <$> good)

