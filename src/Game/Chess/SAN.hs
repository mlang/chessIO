{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-|
Module      : Game.Chess.SAN
Description : Standard Algebraic Notation
Copyright   : (c) Mario Lang, 2021
License     : BSD3
Maintainer  : mlang@blind.guru
Stability   : experimental

Parsers and printers for [Algebraic Notation](https://en.wikipedia.org/wiki/Algebraic_notation_%28chess%29).
-}
module Game.Chess.SAN (
  -- * Conversion
  fromSAN, toSAN, unsafeToSAN
  -- * Parsers
, SANToken, strictSAN, relaxedSAN
  -- * Utilities
, varToSAN
) where

import           Control.Applicative        (Applicative (liftA2))
import           Control.Arrow              ((&&&))
import           Control.Lens               (view)
import           Control.Lens.Iso           (from)
import           Data.Bifunctor             (first)
import qualified Data.ByteString            as Strict (ByteString)
import qualified Data.ByteString.Lazy       as Lazy (ByteString)
import           Data.Char                  (ord)
import           Data.Functor               (($>))
import           Data.List                  (sortOn)
import           Data.List.Extra            (chunksOf)
import           Data.Maybe                 (fromJust)
import           Data.MonoTraversable       (Element, MonoFoldable (otoList))
import           Data.Ord                   (Down (..))
import           Data.Proxy                 (Proxy (..))
import           Data.String                (IsString (fromString))
import qualified Data.Text                  as Strict (Text)
import qualified Data.Text.Lazy             as Lazy (Text)
import           Data.Traversable           (mapAccumL)
import qualified Data.Vector.Unboxed        as Vector
import           Data.Void                  (Void)
import           Data.Word                  (Word8)
import           GHC.Stack                  (HasCallStack)
import           Game.Chess.Internal        (Castle (Kingside, Queenside),
                                             Color (Black, White), PieceType,
                                             Ply, Position (color, moveNumber),
                                             bKscm, bQscm, canCastleKingside,
                                             canCastleQueenside, doPly, inCheck,
                                             isCapture, legalPlies, legalPlies',
                                             pattern Bishop, pattern King,
                                             pattern Knight, pattern Pawn,
                                             pattern Queen, pattern Rook,
                                             pieceAt, plySource, plyTarget,
                                             promoteTo, unpack, unsafeDoPly,
                                             wKscm, wQscm)
import           Game.Chess.Internal.Square
import           Text.Megaparsec            (MonadParsec (token, try), Parsec,
                                             Stream, Token, Tokens,
                                             TraversableStream, VisualStream,
                                             choice, chunk, chunkLength, empty,
                                             errorBundlePretty, option,
                                             optional, parse, (<?>), (<|>))

type Parser s = Parsec Void s

castling :: (Stream s, IsString (Tokens s))
         => Position -> Parser s Ply
castling pos
  | ccks && ccqs = queenside <|> kingside
  | ccks = kingside
  | ccqs = queenside
  | otherwise = empty
 where
  ccks = canCastleKingside pos
  ccqs = canCastleQueenside pos
  kingside = chunk "O-O" $> castleMove Kingside
  queenside = chunk "O-O-O" $> castleMove Queenside
  castleMove Kingside | color pos == White = wKscm
                      | otherwise          = bKscm
  castleMove Queenside | color pos == White = wQscm
                       | otherwise          = bQscm

data From = F File
          | R Rank
          | RF Square
          deriving (Eq, Show)

data SANStatus = Check | Checkmate deriving (Eq, Read, Show)

class SANToken a where
  sanPieceToken :: a -> Maybe PieceType
  fileToken :: a -> Maybe Int
  rankToken :: a -> Maybe Int
  promotionPieceToken :: a -> Maybe PieceType
  statusToken :: a -> Maybe SANStatus

sanPiece :: (Stream s, SANToken (Token s)) => Parser s PieceType
sanPiece = token sanPieceToken mempty <?> "piece"

fileP :: (Stream s, SANToken (Token s)) => Parser s File
fileP = mkFile <$> token fileToken mempty <?> "file"
rankP :: (Stream s, SANToken (Token s)) => Parser s Rank
rankP = mkRank <$> token rankToken mempty <?> "rank"
squareP :: (Stream s, SANToken (Token s)) => Parser s Square
squareP = liftA2 (flip . curry $ view (from rankFile)) fileP rankP <?> "square"

promotionPiece :: (Stream s, SANToken (Token s)) => Parser s PieceType
promotionPiece = token promotionPieceToken mempty <?> "Q, R, B, N"

sanStatus :: (Stream s, SANToken (Token s)) => Parser s SANStatus
sanStatus = token statusToken mempty <?> "+, #"

instance SANToken Char where
  sanPieceToken = \case
    'N' -> Just Knight
    'B' -> Just Bishop
    'R' -> Just Rook
    'Q' -> Just Queen
    'K' -> Just King
    _   -> Nothing
  fileToken c | c >= 'a' && c <= 'h' = Just $ ord c - ord 'a'
              | otherwise  = Nothing
  rankToken c | c >= '1' && c <= '8' = Just $ ord c - ord '1'
              | otherwise  = Nothing
  promotionPieceToken = \case
    'N' -> Just Knight
    'B' -> Just Bishop
    'R' -> Just Rook
    'Q' -> Just Queen
    _   -> Nothing
  statusToken = \case
    '+' -> Just Check
    '#' -> Just Checkmate
    _   -> Nothing

instance SANToken Word8 where
  sanPieceToken = \case
    78 -> Just Knight
    66 -> Just Bishop
    82 -> Just Rook
    81 -> Just Queen
    75 -> Just King
    _  -> Nothing
  rankToken c | c >= 49 && c <= 56 = Just . fromIntegral $ c - 49
              | otherwise  = Nothing
  fileToken c | c >= 97 && c <= 104 = Just . fromIntegral $ c - 97
              | otherwise  = Nothing
  promotionPieceToken = \case
    78 -> Just Knight
    66 -> Just Bishop
    82 -> Just Rook
    81 -> Just Queen
    _  -> Nothing
  statusToken = \case
    43 -> Just Check
    35 -> Just Checkmate
    _  -> Nothing

strictSAN :: forall s. (Stream s, SANToken (Token s), IsString (Tokens s))
          => Position -> Parser s Ply
strictSAN pos = case legalPlies pos of
  [] -> fail "No legal moves in this position"
  ms -> (castling pos <|> normal ms) >>= checkStatus
 where
  normal ms = do
    p <- sanPiece <|> pure Pawn
    case filter (pieceFrom p) ms of
      [] -> fail $ show (color pos) <> " has no " <> show p <> " which could be moved"
      ms' -> target p ms'
  pieceFrom p (plySource -> src) = p == snd (fromJust (pieceAt pos src))
  target p ms = coords p ms >>= \m@(plyTarget -> to) -> case p of
    Pawn | lastRank to -> promoteTo m <$> promotion
    _                  -> pure m
  coords p ms = choice $ fmap (uncurry (<$) . fmap chunk) $
    sortOn (Down . chunkLength (Proxy :: Proxy s) . snd) $
    (\m -> (m, sanCoords pos (p,ms) m)) <$> ms
  promotion = chunk "=" *> promotionPiece
  lastRank (rank -> r) = r == Rank1 || r == Rank8
  checkStatus m
    | inCheck (color nextPos) nextPos && null (legalPlies nextPos)
    = chunk "#" $> m
    | inCheck (color nextPos) nextPos
    = chunk "+" $> m
    | otherwise
    = pure m
   where
    nextPos = unsafeDoPly pos m

relaxedSAN :: (Stream s, SANToken (Token s), IsString (Tokens s))
           => Position -> Parser s Ply
relaxedSAN pos = (castling pos <|> normal) <* optional sanStatus where
  normal = do
    pc <- sanPiece <|> pure Pawn
    (src, _, dst) <- conv <$> location
    prm <- optional $ optional (chunk "=") *> promotionPiece
    case possible pc src dst prm of
      cand | Vector.length cand == 1 -> pure $ Vector.unsafeIndex cand 0
           | Vector.length cand == 0 -> fail "Illegal move"
      _ -> fail "Ambiguous move"
  conv (Nothing, Nothing, cap, to) = (Nothing, cap, to)
  conv (Just f, Nothing, cap, to) = (Just (F f), cap, to)
  conv (Nothing, Just r, cap, to) = (Just (R r), cap, to)
  conv (Just f, Just r, cap, to) = (Just (RF (view (from rankFile) (r, f))), cap, to)
  location = try ((,Nothing,,) <$> (Just <$> fileP) <*> capture <*> squareP)
         <|> try ((Nothing,,,) <$> (Just <$> rankP) <*> capture <*> squareP)
         <|> try ((,,,) <$> (Just <$> fileP) <*> (Just <$> rankP)
                        <*> capture <*> squareP)
         <|>      (Nothing,Nothing,,) <$> capture <*> squareP
  capture = option False $ chunk "x" $> True
  ms = legalPlies' pos
  possible pc src dst prm = Vector.filter (f src) ms where
    f (Just (RF sq)) (unpack -> (src', dst', prm')) =
      pAt src' == pc && src' == sq && dst' == dst && prm' == prm
    f (Just (F ff)) (unpack -> (src', dst', prm')) =
      pAt src' == pc && file src' == ff && dst == dst' && prm == prm'
    f (Just (R fr)) (unpack -> (src', dst', prm')) =
      pAt src' == pc && rank src' == fr && dst == dst' && prm == prm'
    f Nothing (unpack -> (src', dst', prm')) =
      pAt src' == pc && dst == dst' && prm == prm'
  pAt = snd . fromJust . pieceAt pos

fromSAN :: (VisualStream s, TraversableStream s, SANToken (Token s), IsString (Tokens s))
        => Position -> s -> Either String Ply
fromSAN pos = first errorBundlePretty . parse (relaxedSAN pos) ""

toSAN :: (HasCallStack, IsString s) => Position -> Ply -> s
toSAN pos m
  | m `elem` legalPlies pos = fromString $ unsafeToSAN pos m
  | otherwise               = error "Game.Chess.toSAN: Illegal move"

varToSAN :: (MonoFoldable variation, Element variation ~ Ply, IsString string)
         => Position -> variation -> string
varToSAN p = fromString . go p . otoList where
  go _ [] = ""
  go pos plies@(ply:plies')
    | color pos == Black && plies' == []
    = show (moveNumber pos) <> "..." <> toSAN pos ply
    | color pos == Black
    = show (moveNumber pos) <> "..." <> toSAN pos ply <> " " <> fromWhite (doPly pos ply) plies'
    | otherwise
    = fromWhite pos plies
  fromWhite pos = unwords . concat
                . zipWith f [moveNumber pos ..] . chunksOf 2 . snd
                . mapAccumL (curry (uncurry doPly &&& uncurry toSAN)) pos
  f n (x:xs) = (show n <> "." <> x):xs
  f _ []     = []

sanCoords :: IsString s => Position -> (PieceType, [Ply]) -> Ply -> s
sanCoords pos (pc,lms) m@(unpack -> (src, dst, _)) =
  fromString $ source <> target
 where
  capture = isCapture pos m
  source
    | pc == Pawn && capture       = [fileChar src]
    | pc == Pawn                  = []
    | length ms == 1              = []
    | length (filter fEq ms) == 1 = [fileChar src]
    | length (filter rEq ms) == 1 = [rankChar src]
    | otherwise                   = toCoord src
  target
    | capture   = "x" <> toCoord dst
    | otherwise = toCoord dst
  ms = filter ((dst ==) . plyTarget) lms
  fEq (file . plySource -> fl) = fl == srcFile
  rEq (rank . plySource -> rnk) = rnk == srcRank
  (srcRank, srcFile) = view rankFile src

unsafeToSAN :: Position -> Ply -> String
unsafeToSAN pos m@(unpack -> (src, dst, promo)) =
  moveStr <> status
 where
  moveStr = case piece of
    Pawn | capture -> fileChar src : target <> promotion
         | otherwise -> target <> promotion
    King | color pos == White && m == wKscm -> "O-O"
         | color pos == White && m == wQscm -> "O-O-O"
         | color pos == Black && m == bKscm -> "O-O"
         | color pos == Black && m == bQscm -> "O-O-O"
         | otherwise -> 'K' : target
    Knight -> 'N' : source <> target
    Bishop -> 'B' : source <> target
    Rook   -> 'R' : source <> target
    Queen  -> 'Q' : source <> target
  piece = snd (fromJust (pieceAt pos src))
  capture = isCapture pos m
  source
    | length ms == 1              = []
    | length (filter fEq ms) == 1 = [fileChar src]
    | length (filter rEq ms) == 1 = [rankChar src]
    | otherwise                   = toCoord src
  target
    | capture = "x" <> toCoord dst
    | otherwise = toCoord dst
  promotion = case promo of
    Just Knight -> "N"
    Just Bishop -> "B"
    Just Rook   -> "R"
    Just Queen  -> "Q"
    _           -> ""
  status | inCheck (color nextPos) nextPos && null (legalPlies nextPos) = "#"
         | inCheck (color nextPos) nextPos                              = "+"
         | otherwise                                                    = ""
  nextPos = unsafeDoPly pos m
  ms = filter movesTo $ legalPlies pos
  movesTo (unpack -> (src', dst', _)) =
    fmap snd (pieceAt pos src') == Just piece && dst' == dst
  fEq (file . plySource -> thisFile) = thisFile == srcFile
  rEq (rank . plySource -> thisRank) = thisRank == srcRank
  (srcRank, srcFile) = view rankFile src

{-# SPECIALISE relaxedSAN :: Position -> Parser Strict.ByteString Ply #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser Lazy.ByteString Ply #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser Strict.Text Ply #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser Lazy.Text Ply #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser String Ply #-}
{-# SPECIALISE strictSAN :: Position -> Parser Strict.ByteString Ply #-}
{-# SPECIALISE strictSAN :: Position -> Parser Lazy.ByteString Ply #-}
{-# SPECIALISE strictSAN :: Position -> Parser Strict.Text Ply #-}
{-# SPECIALISE strictSAN :: Position -> Parser Lazy.Text Ply #-}
{-# SPECIALISE strictSAN :: Position -> Parser String Ply #-}
