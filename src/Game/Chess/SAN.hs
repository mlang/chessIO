{-# LANGUAGE PolyKinds, FlexibleInstances, GADTs, ScopedTypeVariables #-}
{-|
Module      : Game.Chess.SAN
Description : Standard Algebraic Notation
Copyright   : (c) Mario Lang, 2020
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

import           Control.Applicative (Applicative(liftA2))
import           Control.Arrow ((&&&))
import           Data.Bifunctor (first)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import           Data.Char (chr, ord)
import           Data.Functor (($>))
import           Data.List (sortOn)
import           Data.List.Extra (chunksOf)
import           Data.Maybe (fromJust)
import           Data.Ord (Down(..))
import           Data.Proxy ( Proxy(..) )
import           Data.String (IsString(fromString))
import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import           Data.Traversable (mapAccumL)
import           Data.Void (Void)
import           Data.Word ( Word8 )
import Game.Chess.Internal ( Castle(Queenside, Kingside),
                             Ply, Position(color, moveNumber), Color(Black, White),
                             PieceType(..), isCapture, pieceAt,
                             promoteTo, plySource, plyTarget, unpack, doPly, unsafeDoPly, legalPlies,
                             inCheck, canCastleKingside, canCastleQueenside,
                             wKscm, wQscm, bKscm, bQscm )
import Game.Chess.Internal.Square (IsSquare(..), fileChar, rankChar, toCoord)
import Text.Megaparsec ( optional, (<|>), empty, (<?>), chunk, parse,
                         errorBundlePretty, choice, option, Parsec,
                         MonadParsec(try, token),
                         Stream, TraversableStream, VisualStream,
                         Token, Tokens, chunkLength )
import GHC.Stack (HasCallStack)

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

data From = File Int
          | Rank Int
          | Square Int
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

fileP, rankP, squareP :: (Stream s, SANToken (Token s)) => Parser s Int
fileP = token fileToken mempty <?> "file"
rankP = token rankToken mempty <?> "rank"
squareP = liftA2 (flip $ curry toIndex) fileP rankP <?> "square"

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
    _ -> Nothing
  fileToken c | c >= 'a' && c <= 'h' = Just $ ord c - ord 'a'
              | otherwise  = Nothing
  rankToken c | c >= '1' && c <= '8' = Just $ ord c - ord '1'
              | otherwise  = Nothing
  promotionPieceToken = \case
    'N' -> Just Knight
    'B' -> Just Bishop
    'R' -> Just Rook
    'Q' -> Just Queen
    _ -> Nothing
  statusToken = \case
    '+' -> Just Check
    '#' -> Just Checkmate
    _ -> Nothing

instance SANToken Word8 where
  sanPieceToken = \case
    78 -> Just Knight
    66 -> Just Bishop
    82 -> Just Rook
    81 -> Just Queen
    75 -> Just King
    _ -> Nothing
  rankToken c | c >= 49 && c <= 56 = Just . fromIntegral $ c - 49
              | otherwise  = Nothing
  fileToken c | c >= 97 && c <= 104 = Just . fromIntegral $ c - 97
              | otherwise  = Nothing
  promotionPieceToken = \case
    78 -> Just Knight
    66 -> Just Bishop
    82 -> Just Rook
    81 -> Just Queen
    _ -> Nothing
  statusToken = \case
    43 -> Just Check
    35 -> Just Checkmate
    _ -> Nothing

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
  pieceFrom p (plySource -> from) = p == snd (fromJust (pieceAt pos from))
  target p ms = coords p ms >>= \m@(plyTarget -> to) -> case p of
    Pawn | lastRank to -> promoteTo m <$> promotion
    _ -> pure m
  coords p ms = choice $ fmap (uncurry (<$) . fmap chunk) $
    sortOn (Down . chunkLength (Proxy :: Proxy s) . snd) $
    (\m -> (m, sanCoords pos (p,ms) m)) <$> ms
  promotion = chunk "=" *> promotionPiece
  lastRank i = i >= 56 || i <= 7
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
    (from, _, to) <- conv <$> location
    prm <- optional $ optional (chunk "=") *> promotionPiece
    case possible pc from to prm of
      [m] -> pure m
      [] -> fail "Illegal move"
      _ -> fail "Ambiguous move"
  conv (Nothing, Nothing, cap, to) = (Nothing, cap, to)
  conv (Just f, Nothing, cap, to) = (Just (File f), cap, to)
  conv (Nothing, Just r, cap, to) = (Just (Rank r), cap, to)
  conv (Just f, Just r, cap, to) = (Just (Square (r*8+f)), cap, to)
  location = try ((,Nothing,,) <$> (Just <$> fileP) <*> capture <*> squareP)
         <|> try ((Nothing,,,) <$> (Just <$> rankP) <*> capture <*> squareP)
         <|> try ((,,,) <$> (Just <$> fileP) <*> (Just <$> rankP)
                        <*> capture <*> squareP)
         <|>      (Nothing,Nothing,,) <$> capture <*> squareP
  capture = option False $ chunk "x" $> True
  ms = legalPlies pos
  possible pc from to prm = filter (f from) ms where
    f (Just (Square sq)) (unpack -> (from', to', prm')) =
      pAt from' == pc && from' == sq && to' == to && prm' == prm
    f (Just (File ff)) (unpack -> (from', to', prm')) =
      pAt from' == pc && fileOf from' == ff && to == to' && prm == prm'
    f (Just (Rank fr)) (unpack -> (from', to', prm')) =
      pAt from' == pc && rankOf from' == fr && to == to' && prm == prm'
    f Nothing (unpack -> (from', to', prm')) =
      pAt from' == pc && to == to' && prm == prm'
  pAt = snd . fromJust . pieceAt pos

fromSAN :: (VisualStream s, TraversableStream s, SANToken (Token s), IsString (Tokens s))
        => Position -> s -> Either String Ply
fromSAN pos = first errorBundlePretty . parse (relaxedSAN pos) ""

toSAN :: (HasCallStack, IsString s) => Position -> Ply -> s
toSAN pos m
  | m `elem` legalPlies pos = fromString $ unsafeToSAN pos m
  | otherwise               = error "Game.Chess.toSAN: Illegal move"

varToSAN :: IsString s => Position -> [Ply] -> s
varToSAN _ [] = ""
varToSAN pos plies
  | color pos == Black && length plies == 1
  = fromString $ show (moveNumber pos) <> "..." <> toSAN pos (head plies)
  | color pos == Black
  = fromString $ show (moveNumber pos) <> "..." <> toSAN pos (head plies) <> " " <> fromWhite (doPly pos (head plies)) (tail plies)
  | otherwise
  = fromString $ fromWhite pos plies
 where
  fromWhite pos' = unwords . concat
                 . zipWith f [moveNumber pos' ..] . chunksOf 2 . snd
                 . mapAccumL (curry (uncurry doPly &&& uncurry toSAN)) pos'
  f n (x:xs) = (show n <> "." <> x):xs
  f _ [] = []

sanCoords :: IsString s => Position -> (PieceType, [Ply]) -> Ply -> s
sanCoords pos (pc,lms) m@(unpack -> (from, to, _)) =
  fromString $ source <> target
 where
  capture = isCapture pos m
  source
    | pc == Pawn && capture       = [fileChar from]
    | pc == Pawn                  = []
    | length ms == 1              = []
    | length (filter fEq ms) == 1 = [fileChar from]
    | length (filter rEq ms) == 1 = [rankChar from]
    | otherwise                   = toCoord from
  target
    | capture   = "x" <> toCoord to
    | otherwise = toCoord to
  ms = filter ((to ==) . plyTarget) lms
  fEq (fileOf . plySource -> file) = file == fromFile
  rEq (rankOf . plySource -> rank) = rank == fromRank
  (fromRank, fromFile) = toRF from

unsafeToSAN :: Position -> Ply -> String
unsafeToSAN pos m@(unpack -> (from, to, promo)) =
  moveStr <> status
 where
  moveStr = case piece of
    Pawn | capture -> fileChar from : target <> promotion
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
  piece = fromJust $ snd <$> pieceAt pos from
  capture = isCapture pos m
  source
    | length ms == 1              = []
    | length (filter fEq ms) == 1 = [fileChar from]
    | length (filter rEq ms) == 1 = [rankChar from]
    | otherwise                   = toCoord from
  target
    | capture = "x" <> toCoord to
    | otherwise = toCoord to
  promotion = case promo of
    Just Knight -> "N"
    Just Bishop -> "B"
    Just Rook   -> "R"
    Just Queen  -> "Q"
    _      -> ""
  status | inCheck (color nextPos) nextPos && null (legalPlies nextPos) = "#"
         | inCheck (color nextPos) nextPos                              = "+"
         | otherwise                                                    = ""
  nextPos = unsafeDoPly pos m
  ms = filter movesTo $ legalPlies pos
  movesTo (unpack -> (from', to', _)) =
    fmap snd (pieceAt pos from') == Just piece && to' == to
  fEq (fileOf . plySource -> file) = file == fromFile
  rEq (rankOf . plySource -> rank) = rank == fromRank
  (fromRank, fromFile) = toRF from

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
