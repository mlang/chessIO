{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, GADTs, ScopedTypeVariables #-}
{-|
Module      : Game.Chess
Description : Basic data types and functions related to the game of chess
Copyright   : (c) Mario Lang, 2019
License     : BSD3
Maintainer  : mlang@blind.guru
Stability   : experimental

A small collection of data types and functions to represent Chess positions
and moves including move generation and parsing from external sources.

This module does deliberately not implement
any search or evaluation functionality.  It is intended to be used
to lay the ground for communicating with other programs or players, hence the
package name chessIO.
-}
module Game.Chess (
  -- * Chess positions
  Color(..), opponent
, Sq(..), isLight, isDark
, PieceType(..), Castle(..)
, Position, startpos, color, moveNumber, pieceAt, inCheck
, castlingRights, canCastleKingside, canCastleQueenside
  -- ** Converting from/to Forsyth-Edwards-Notation
, fromFEN, toFEN
  -- * Chess moves
, Move
  -- ** Converting from/to algebraic notation
, strictSAN, relaxedSAN, fromSAN, toSAN, unsafeToSAN, fromUCI, toUCI
  -- ** Move generation
, moves
  -- ** Executing moves
, applyMove, unsafeApplyMove
) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad
import Data.Binary
import Data.Bits
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Char
import Data.Functor (($>))
import Data.Ix
import Data.List
import Data.Maybe
import Data.Ord (Down(..))
import Data.Proxy
import Data.String
import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import Data.Vector.Unboxed (Vector, (!))
import Data.Void
import qualified Data.Vector.Unboxed as Vector
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)
import Data.Tree
import Data.Tuple

positionTree :: Position -> Tree Position
positionTree pos = Node pos $ positionForest pos
positionForest pos = positionTree . unsafeApplyMove pos <$> moves pos
plyForest :: Position -> Forest Move
plyForest pos = plyTree pos <$> moves pos
plyTree pos ply = Node ply . plyForest $ unsafeApplyMove pos ply

type Parser s = Parsec Void s

castling :: (Stream s, IsString (Tokens s))
         => Position -> Parser s Move
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
          deriving (Show)

capturing :: Position -> Move -> Maybe PieceType
capturing pos@Position{flags} (unpack -> (_, to, _))
  | (flags .&. epMask) `testBit` to = Just Pawn
  | otherwise = snd <$> pieceAt pos to

isCapture :: Position -> Move -> Bool
isCapture pos = isJust . capturing pos

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
squareP = liftA2 (\f r -> r*8+f) fileP rankP <?> "square"

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
          => Position -> Parser s Move
strictSAN pos@Position{flags} = case moves pos of
  [] -> fail $ "No legal moves in this position"
  ms -> (castling pos <|> normal ms) >>= checkStatus
 where
  normal ms = do
    p <- sanPiece <|> pure Pawn
    case filter (pieceFrom p) ms of
      [] -> fail $ show (color pos) <> " has no " <> show p <> " which could be moved"
      ms' -> target p ms'
  pieceFrom p (moveFrom -> from) = p == snd (fromJust (pieceAt pos from))
  moveFrom (unpack -> (from, _, _)) = from
  target p ms = coords p ms >>= \m@(unpack -> (_, to, _)) -> case p of
    Pawn | lastRank to -> promoteTo m <$> promotion
    _ -> pure m
  coords p ms = choice $ fmap (uncurry (<$) . fmap chunk) $
    sortOn (Down . chunkLength (Proxy :: Proxy s) . snd) $
    (\m -> (m, sanCoords pos (p,ms) m)) <$> ms
  promotion = chunk "=" *> promotionPiece
  lastRank i = i >= 56 || i <= 7
  checkStatus m
    | inCheck (color nextPos) nextPos && null (moves nextPos)
    = chunk "#" $> m
    | inCheck (color nextPos) nextPos
    = chunk "+" $> m
    | otherwise
    = pure m
   where
    nextPos = unsafeApplyMove pos m

relaxedSAN :: (Stream s, SANToken (Token s), IsString (Tokens s))
           => Position -> Parser s Move
relaxedSAN pos = (castling pos <|> normal) <* optional sanStatus where
  normal = do
    pc <- sanPiece <|> pure Pawn
    (from, cap, to) <- conv <$> location
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
  ms = moves pos
  possible pc from to prm = filter (f from) ms where
    f (Just (Square from)) (unpack -> (from', to', prm')) =
      pAt from' == pc && from' == from && to' == to && prm' == prm
    f (Just (File ff)) (unpack -> (from', to', prm')) =
      pAt from' == pc && from' `mod` 8 == ff && to == to' && prm == prm'
    f (Just (Rank fr)) (unpack -> (from', to', prm')) =
      pAt from' == pc && from' `div` 8 == fr && to == to' && prm == prm'
    f Nothing (unpack -> (from', to', prm')) =
      pAt from' == pc && to == to' && prm == prm'
  pAt = snd . fromJust . pieceAt pos

fromSAN :: (Stream s, SANToken (Token s), IsString (Tokens s))
        => Position -> s -> Either String Move
fromSAN pos s = case parse (relaxedSAN pos) "" s of
  Right m -> Right m
  Left err -> Left $ errorBundlePretty err

toSAN :: Position -> Move -> String
toSAN pos m | m `elem` moves pos = unsafeToSAN pos m
            | otherwise          = error "Game.Chess.toSAN: Illegal move"

sanCoords :: IsString s => Position -> (PieceType, [Move]) -> Move -> s
sanCoords pos@Position{flags} (pc,lms) m@(unpack -> (from, to, _)) =
  fromString $ source <> target
 where
  capture = isCapture pos m
  source
    | pc == Pawn && capture
    = [fileChar from]
    | pc == Pawn
    = []
    | length ms == 1
    = []
    | length (filter fEq ms) == 1
    = [fileChar from]
    | length (filter rEq ms) == 1
    = [rankChar from]
    | otherwise
    = toCoord from
  target
    | capture = "x" <> toCoord to
    | otherwise = toCoord to
  ms = filter (isMoveTo to) $ lms
  isMoveTo to (unpack -> (_, to', _)) = to == to'
  fEq (unpack -> (from', _, _)) = from' `mod` 8 == fromFile
  rEq (unpack -> (from', _, _)) = from' `div` 8 == fromRank
  (fromRank, fromFile) = toRF from
  fileChar i = chr $ (i `mod` 8) + ord 'a'
  rankChar i = chr $ (i `div` 8) + ord '1'

unsafeToSAN :: Position -> Move -> String
unsafeToSAN pos@Position{flags} m@(unpack -> (from, to, promo)) =
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
  status | inCheck (color nextPos) nextPos && null (moves nextPos)
         = "#"
         | inCheck (color nextPos) nextPos
         = "+"
         | otherwise
         = ""
  nextPos = unsafeApplyMove pos m
  ms = filter movesTo $ moves pos
  movesTo (unpack -> (from', to', _)) =
    fmap snd (pieceAt pos from') == Just piece && to' == to
  fEq (unpack -> (from', _, _)) = from' `mod` 8 == fromFile
  rEq (unpack -> (from', _, _)) = from' `div` 8 == fromRank
  (fromRank, fromFile) = toRF from
  fileChar i = chr $ (i `mod` 8) + ord 'a'
  rankChar i = chr $ (i `div` 8) + ord '1'

-- | The starting position as given by the FEN string
--   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".
startpos :: Position
startpos = fromJust $
  fromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ix, Ord, Show)

data Color = Black | White deriving (Eq, Ix, Ord, Show)

pieceAt :: IsSquare sq => Position -> sq -> Maybe (Color, PieceType)
pieceAt (board -> BB{wP, wN, wB, wR, wQ, wK, bP, bN, bB, bR, bQ, bK}) (toIndex -> sq)
  | wP `testBit` sq = Just (White, Pawn)
  | wN `testBit` sq = Just (White, Knight)
  | wB `testBit` sq = Just (White, Bishop)
  | wR `testBit` sq = Just (White, Rook)
  | wQ `testBit` sq = Just (White, Queen)
  | wK `testBit` sq = Just (White, King)
  | bP `testBit` sq = Just (Black, Pawn)
  | bN `testBit` sq = Just (Black, Knight)
  | bB `testBit` sq = Just (Black, Bishop)
  | bR `testBit` sq = Just (Black, Rook)
  | bQ `testBit` sq = Just (Black, Queen)
  | bK `testBit` sq = Just (Black, King)
  | otherwise       = Nothing

opponent :: Color -> Color
opponent White = Black
opponent Black = White

data Piece = Piece !Color !PieceType deriving (Eq, Show)

data Sq = A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1
        | A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2
        | A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3
        | A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4
        | A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5
        | A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6
        | A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7
        | A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8
        deriving (Bounded, Enum, Eq, Ix, Ord, Show)

class IsSquare sq where
  toIndex :: sq -> Int

toRF :: IsSquare sq => sq -> (Int, Int)
toRF sq = toIndex sq `divMod` 8

toCoord :: (IsSquare sq, IsString s) => sq -> s
toCoord (toRF -> (r,f)) = fromString $ [chr (f + ord 'a'), chr (r + ord '1')]

instance IsSquare Sq where
  toIndex = fromEnum

instance IsSquare Int where
  toIndex = id

isDark :: IsSquare sq => sq -> Bool
isDark (toIndex -> sq) = (0xaa55aa55aa55aa55 :: Word64) `testBit` sq

isLight :: IsSquare sq => sq -> Bool
isLight = not . isDark

data BB = BB { wP, wN, wB, wR, wQ, wK :: !Word64
             , bP, bN, bB, bR, bQ, bK :: !Word64
             } deriving (Eq, Show)

data Position = Position {
  board :: !BB
, color :: !Color
  -- ^ active color
, flags :: !Word64
, halfMoveClock :: !Int
, moveNumber :: !Int
  -- ^ number of the full move
}

instance Eq Position where
  a == b = board a == board b && color a == color b && flags a == flags b

emptyBB :: BB
emptyBB = BB 0 0 0 0 0 0 0 0 0 0 0 0

-- | Construct a position from Forsyth-Edwards-Notation.
fromFEN :: String -> Maybe Position
fromFEN fen
  | length parts /= 6
  = Nothing
  | otherwise =
    Position <$> readBoard (parts !! 0)
             <*> readColor (parts !! 1)
             <*> readFlags (parts !! 2) (parts !! 3)
             <*> readMaybe (parts !! 4)
             <*> readMaybe (parts !! 5)
 where
  parts = words fen
  readBoard = go (toRF A8) emptyBB where
    go rf@(r,f) bb ('r':s) = go (r, f + 1) (bb { bR = bR bb .|. rfBit rf }) s
    go rf@(r,f) bb ('n':s) = go (r, f + 1) (bb { bN = bN bb .|. rfBit rf }) s
    go rf@(r,f) bb ('b':s) = go (r, f + 1) (bb { bB = bB bb .|. rfBit rf }) s
    go rf@(r,f) bb ('q':s) = go (r, f + 1) (bb { bQ = bQ bb .|. rfBit rf }) s
    go rf@(r,f) bb ('k':s) = go (r, f + 1) (bb { bK = bK bb .|. rfBit rf }) s
    go rf@(r,f) bb ('p':s) = go (r, f + 1) (bb { bP = bP bb .|. rfBit rf }) s
    go rf@(r,f) bb ('R':s) = go (r, f + 1) (bb { wR = wR bb .|. rfBit rf }) s
    go rf@(r,f) bb ('N':s) = go (r, f + 1) (bb { wN = wN bb .|. rfBit rf }) s
    go rf@(r,f) bb ('B':s) = go (r, f + 1) (bb { wB = wB bb .|. rfBit rf }) s
    go rf@(r,f) bb ('Q':s) = go (r, f + 1) (bb { wQ = wQ bb .|. rfBit rf }) s
    go rf@(r,f) bb ('K':s) = go (r, f + 1) (bb { wK = wK bb .|. rfBit rf }) s
    go rf@(r,f) bb ('P':s) = go (r, f + 1) (bb { wP = wP bb .|. rfBit rf }) s
    go (r,f) bb ('1':s) = go (r, f + 1) bb s
    go (r,f) bb ('2':s) = go (r, f + 2) bb s
    go (r,f) bb ('3':s) = go (r, f + 3) bb s
    go (r,f) bb ('4':s) = go (r, f + 4) bb s
    go (r,f) bb ('5':s) = go (r, f + 5) bb s
    go (r,f) bb ('6':s) = go (r, f + 6) bb s
    go (r,f) bb ('7':s) = go (r, f + 7) bb s
    go (r,f) bb ('8':s) = go (r, f + 8) bb s
    go (r,_) bb ('/':s) = go (r - 1, 0) bb s
    go _ bb [] = Just bb
    go _ _ _ = Nothing

  readColor "w" = Just White
  readColor "b" = Just Black
  readColor _ = Nothing

  readFlags cst ep = (.|.) <$> readCst cst <*> readEP ep where
    readCst "-" = Just 0
    readCst x = go x where
      go ('K':xs) = (crwKs .|.) <$> go xs
      go ('Q':xs) = (crwQs .|.) <$> go xs
      go ('k':xs) = (crbKs .|.) <$> go xs
      go ('q':xs) = (crbQs .|.) <$> go xs
      go [] = Just 0
      go _ = Nothing
    readEP "-" = Just 0
    readEP [f,r]
      | inRange ('a','h') f && (r == '3' || r == '6')
      = Just $ bit ((ord r - ord '1') * 8 + (ord f - ord 'a'))
    readEP _ = Nothing

rfBit :: Bits bits => (Int, Int) -> bits
rfBit (r,f) | inRange (0,7) r && inRange (0,7) f = bit $ r*8 + f
            | otherwise = error $ "Out of range: " <> show r <> " " <> show f

-- | Convert a position to Forsyth-Edwards-Notation.
toFEN :: Position -> String
toFEN (Position bb c flgs hm mn) = unwords [
    intercalate "/" (rank <$> [7,6..0])
  , showColor c, showCst (flgs `clearMask` epMask), showEP (flgs .&. epMask), show hm, show mn
  ]
 where
  showColor White = "w"
  showColor Black = "b"
  showCst 0 = "-"
  showCst x = snd . wks . wqs . bks . bqs $ (x, "") where
    wks (v, xs) | v `testMask` crwKs = (v, 'K':xs)
                | otherwise          = (v, xs)
    wqs (v, xs) | v `testMask` crwQs = (v, 'Q':xs)
                | otherwise          = (v, xs)
    bks (v, xs) | v `testMask` crbKs = (v, 'k':xs)
                | otherwise          = (v, xs)
    bqs (v, xs) | v `testMask` crbQs = (v, 'q':xs)
                | otherwise          = (v, xs)
  showEP 0 = "-"
  showEP x = chr (f + ord 'a') : [chr (r + ord '1')] where
    (r, f) = toRF $ bitScanForward x
  rank r = concatMap countEmpty $ groupBy (\x y -> x == y && x == ' ') $
           charAt r <$> [0..7]
  countEmpty xs | head xs == ' ' = show $ length xs
                | otherwise = xs
  charAt r f
    | wP bb `testBit` b = 'P'
    | wN bb `testBit` b = 'N'
    | wB bb `testBit` b = 'B'
    | wR bb `testBit` b = 'R'
    | wQ bb `testBit` b = 'Q'
    | wK bb `testBit` b = 'K'
    | bP bb `testBit` b = 'p'
    | bN bb `testBit` b = 'n'
    | bB bb `testBit` b = 'b'
    | bR bb `testBit` b = 'r'
    | bQ bb `testBit` b = 'q'
    | bK bb `testBit` b = 'k'
    | otherwise         = ' '
   where b = r*8 + f

occupiedBy :: Color -> BB -> Word64
occupiedBy White bb = wP bb .|. wN bb .|. wB bb .|. wR bb .|. wQ bb .|. wK bb
occupiedBy Black bb = bP bb .|. bN bb .|. bB bb .|. bR bb .|. bQ bb .|. bK bb

occupied :: BB -> Word64
occupied bb = occupiedBy White bb .|. occupiedBy Black bb

notOccupied :: BB -> Word64
notOccupied = complement . occupied

foldBits :: (a -> Int -> a) -> a -> Word64 -> a
foldBits _ a 0 = a
foldBits f !a n = foldBits f (f a lsb) (n .&. (n-1)) where
  !lsb = countTrailingZeros n

bitScanForward, bitScanReverse :: Word64 -> Int
bitScanForward = countTrailingZeros
bitScanReverse = (63 -) . countLeadingZeros

newtype Move = Move Word16 deriving (Binary, Eq)

instance Show Move where
  show = toUCI

move :: (IsSquare from, IsSquare to) => from -> to -> Move
move (toIndex -> from) (toIndex -> to) =
  Move $ fromIntegral to .|. fromIntegral from `unsafeShiftL` 6

promoteTo :: Move -> PieceType -> Move
promoteTo (Move x) = Move . set where
  set Knight = x .|. 0b001_000000_000000
  set Bishop = x .|. 0b010_000000_000000
  set Rook   = x .|. 0b011_000000_000000
  set Queen  = x .|. 0b100_000000_000000
  set _      = x

unpack :: Move -> (Int, Int, Maybe PieceType)
unpack (Move x) = ( fromIntegral ((x `unsafeShiftR` 6) .&. 0b111111)
                  , fromIntegral (x .&. 0b111111)
                  , piece)
 where
  !piece = case x `unsafeShiftR` 12 of
    1 -> Just Knight
    2 -> Just Bishop
    3 -> Just Rook
    4 -> Just Queen
    _ -> Nothing

-- | Parse a move in the format used by the Universal Chess Interface protocol.
fromUCI :: Position -> String -> Maybe Move
fromUCI pos (fmap (splitAt 2) . splitAt 2 -> (from, (to, promo)))
  | length from == 2 && length to == 2 && null promo
  = move <$> readCoord from <*> readCoord to >>= relativeTo pos
  | length from == 2 && length to == 2 && length promo == 1
  = (\f t p -> move f t `promoteTo` p) <$> readCoord from
                                       <*> readCoord to
                                       <*> readPromo promo
      >>= relativeTo pos
 where
  readCoord [f,r]
    | inRange ('a','h') f && inRange ('1','8') r
    = Just $ (ord r - ord '1') * 8 + (ord f - ord 'a')
  readCoord _ = Nothing
  readPromo "q" = Just Queen
  readPromo "r" = Just Rook
  readPromo "b" = Just Bishop
  readPromo "n" = Just Knight
  readPromo _ = Nothing
fromUCI _ _ = Nothing

-- | Convert a move to the format used by the Universal Chess Interface protocol.
toUCI :: Move -> String
toUCI (unpack -> (from, to, promo)) = coord from <> coord to <> p where
  coord x = let (r,f) = toRF x in
            chr (f + ord 'a') : [chr (r + ord '1')]
  p = case promo of
    Just Queen -> "q"
    Just Rook -> "r"
    Just Bishop -> "b"
    Just Knight -> "n"
    _ -> ""

-- | Validate that a certain move is legal in the given position.
relativeTo :: Position -> Move -> Maybe Move
relativeTo pos m | m `elem` moves pos = Just m
                 | otherwise = Nothing

shiftN, shiftNNE, shiftNE, shiftENE, shiftE, shiftESE, shiftSE, shiftSSE, shiftS, shiftSSW, shiftSW, shiftWSW, shiftW, shiftWNW, shiftNW, shiftNNW :: Word64 -> Word64
shiftN   w = w `unsafeShiftL` 8
shiftNNE w = w `unsafeShiftL` 17 .&. notAFile
shiftNE  w = w `unsafeShiftL` 9 .&. notAFile
shiftENE w = w `unsafeShiftL` 10 .&. notABFile
shiftE   w = w `unsafeShiftL` 1 .&. notAFile
shiftESE w = w `unsafeShiftR` 6 .&. notABFile
shiftSE  w = w `unsafeShiftR` 7 .&. notAFile
shiftSSE w = w `unsafeShiftR` 15 .&. notAFile
shiftS   w = w `unsafeShiftR` 8
shiftSSW w = w `unsafeShiftR` 17 .&. notHFile
shiftSW  w = w `unsafeShiftR` 9 .&. notHFile
shiftWSW w = w `unsafeShiftR` 10 .&. notGHFile
shiftW   w = w `unsafeShiftR` 1 .&. notHFile
shiftWNW w = w `unsafeShiftL` 6 .&. notGHFile
shiftNW  w = w `unsafeShiftL` 7 .&. notHFile
shiftNNW w = w `unsafeShiftL` 15 .&. notHFile

-- | Apply a move to the given position.
--
-- This function checks if the move is actually legal and throws and error
-- if it isn't.  See 'unsafeApplyMove' for a version that omits the legality check.
applyMove :: Position -> Move -> Position
applyMove p m
  | m `elem` moves p = unsafeApplyMove p m
  | otherwise        = error "Game.Chess.applyMove: Illegal move"

-- | An unsafe version of 'applyMove'.  Only use this if you are sure the given move
-- can be applied to the position.  This is useful if the move has been generated
-- by the 'moves' function.
unsafeApplyMove :: Position -> Move -> Position
unsafeApplyMove pos@Position{color = White} m =
  (unsafeApplyMove' pos m) { color = Black }
unsafeApplyMove pos@Position{color = Black, moveNumber} m =
  (unsafeApplyMove' pos m) { color = White, moveNumber = succ moveNumber }

unsafeApplyMove' :: Position -> Move -> Position
unsafeApplyMove' pos@Position{board, flags} m@(unpack -> (from, to, promo))
  | m == wKscm && flags `testMask` crwKs
  = pos { board = board { wK = wK board `xor` mask
                        , wR = wR board `xor` (bit (toIndex H1) `setBit` toIndex F1)
                        }
        , flags = flags `clearMask` (rank1 .|. epMask)
        }
  | m == wQscm && flags `testMask` crwQs
  = pos { board = board { wK = wK board `xor` mask
                        , wR = wR board `xor` (bit (toIndex A1) `setBit` toIndex D1)
                        }
        , flags = flags `clearMask` (rank1 .|. epMask)
        }
  | m == bKscm && flags `testMask` crbKs
  = pos { board = board { bK = bK board `xor` mask
                        , bR = bR board `xor` (bit (toIndex H8) `setBit` toIndex F8)
                        }
        , flags = flags `clearMask` (rank8 .|. epMask)
        }
  | m == bQscm && flags `testMask` crbQs
  = pos { board = board { bK = bK board `xor` mask
                        , bR = bR board `xor` (bit (toIndex A8) `setBit` toIndex D8)
                        }
        , flags = flags `clearMask` (rank8 .|. epMask)
        }
  | Just piece <- promo
  = case color pos of
      White -> case piece of
        Queen -> pos { board = clearB { wP = wP board `clearBit` from
                                      , wQ = wQ board `setBit` to
                                      }
                     , flags = flags `clearMask` (epMask .|. bit to)
                     }
        Rook  -> pos { board = clearB { wP = wP board `clearBit` from
                                      , wR = wR board `setBit` to
                                      }
                     , flags = flags `clearMask` (epMask .|. bit to)
                     }
        Bishop -> pos { board = clearB { wP = wP board `clearBit` from
                                       , wB = wB board `setBit` to
                                       }
                      , flags = flags `clearMask` (epMask .|. bit to)
                      }
        Knight -> pos { board = clearB { wP = wP board `clearBit` from
                                       , wN = wN board `setBit` to
                                       }
                      , flags = flags `clearMask` (epMask .|. bit to)
                      }
        _ -> error "Impossible: White tried to promote to Pawn"
      Black -> case piece of
        Queen -> pos { board = clearW { bP = bP board `clearBit` from
                                      , bQ = bQ board `setBit` to
                                      }
                     , flags = flags `clearMask` (epMask .|. bit to)
                     }  
        Rook   -> pos { board = clearW { bP = bP board `clearBit` from
                                       , bR = bR board `setBit` to
                                       }
                      , flags = flags `clearMask` (epMask .|. bit to)
                      }
        Bishop -> pos { board = clearW { bP = bP board `clearBit` from
                                       , bB = bB board `setBit` to
                                       }
                      , flags = flags `clearMask` (epMask .|. bit to)
                      }
        Knight -> pos { board = clearW { bP = bP board `clearBit` from
                                       , bN = bN board `setBit` to
                                       }
                      , flags = flags `clearMask` (epMask .|. bit to)
                      }
        _ -> error "Impossible: Black tried to promote to Pawn"
  | otherwise
  = pos { board = newBoard
        , flags = (flags `clearMask` (epMask .|. mask)) .|. dpp
        }
 where
  !epBit = case color pos of
    White | wP board `testMask` fromMask -> shiftS $ flags .&. rank6 .&. toMask
    Black | bP board `testMask` fromMask -> shiftN $ flags .&. rank3 .&. toMask
    _ -> 0
  clearW = board { wP = wP board `clearMask` (toMask .|. epBit)
                 , wN = wN board `clearMask` toMask
                 , wB = wB board `clearMask` toMask
                 , wR = wR board `clearMask` toMask
                 , wQ = wQ board `clearMask` toMask
                 }
  clearB = board { bP = bP board `clearMask` (toMask .|. epBit)
                 , bN = bN board `clearMask` toMask
                 , bB = bB board `clearMask` toMask
                 , bR = bR board `clearMask` toMask
                 , bQ = bQ board `clearMask` toMask
                 }
  !fromMask = 1 `unsafeShiftL` from
  !toMask = 1 `unsafeShiftL` to
  !mask = fromMask .|. toMask
  newBoard = case color pos of
    White | wP board `testMask` fromMask -> clearB { wP = wP board `xor` mask }
          | wN board `testMask` fromMask -> clearB { wN = wN board `xor` mask }
          | wB board `testMask` fromMask -> clearB { wB = wB board `xor` mask }
          | wR board `testMask` fromMask -> clearB { wR = wR board `xor` mask }
          | wQ board `testMask` fromMask -> clearB { wQ = wQ board `xor` mask }
          | otherwise -> clearB { wK = wK board `xor` mask }
    Black | bP board `testMask` fromMask -> clearW { bP = bP board `xor` mask }
          | bN board `testMask` fromMask -> clearW { bN = bN board `xor` mask }
          | bB board `testMask` fromMask -> clearW { bB = bB board `xor` mask }
          | bR board `testMask` fromMask -> clearW { bR = bR board `xor` mask }
          | bQ board `testMask` fromMask -> clearW { bQ = bQ board `xor` mask }
          | otherwise -> clearW { bK = bK board `xor` mask }
  dpp = case color pos of
    White | fromMask .&. rank2 .&. wP board /= 0 && from + 16 == to -> shiftN fromMask
    Black | fromMask .&. rank7 .&. bP board /= 0 && from - 16 == to -> shiftS fromMask
    _                                                            -> 0

-- | Generate a list of possible moves for the given position.
moves :: Position -> [Move]
moves pos@Position{color, board, flags} = filter legalMove $
      kingMoves
    . knightMoves
    . slideMoves Queen pos ours notOurs occ
    . slideMoves Rook pos ours notOurs occ
    . slideMoves Bishop pos ours notOurs occ
    . pawnMoves
    $ []
 where
  legalMove = not . inCheck color . unsafeApplyMove' pos
  !ours = occupiedBy color board
  !them = occupiedBy (opponent color) board
  !notOurs = complement ours
  !occ = ours .|. them
  (!pawnMoves, !knightMoves, !kingMoves) = case color of
    White ->
      ( wPawnMoves (wP board) (complement occ) (them .|. (flags .&. epMask))
      , flip (foldBits genNMoves) (wN board)
      , flip (foldBits genKMoves) (wK board) . wShort . wLong)
    Black ->
      ( bPawnMoves (bP board) (complement occ) (them .|. (flags .&. epMask))
      , flip (foldBits genNMoves) (bN board)
      , flip (foldBits genKMoves) (bK board) . bShort . bLong)
  genNMoves ms sq = foldBits (mkM sq) ms ((knightAttacks ! sq) .&. notOurs)
  genKMoves ms sq = foldBits (mkM sq) ms ((kingAttacks ! sq) .&. notOurs)
  wShort ml | canCastleKingside' pos occ = wKscm : ml
            | otherwise             = ml
  wLong ml  | canCastleQueenside' pos occ = wQscm : ml
            | otherwise              = ml
  bShort ml | canCastleKingside' pos occ = bKscm : ml
            | otherwise             = ml
  bLong ml  | canCastleQueenside' pos occ = bQscm : ml
            | otherwise              = ml
  mkM !from ms !to = move from to : ms

-- | Returns 'True' if 'Color' is in check in the given position.
inCheck :: Color -> Position -> Bool
inCheck White Position{board} = attackedBy Black board (occupied board) (bitScanForward (wK board))
inCheck Black Position{board} = attackedBy White board (occupied board) (bitScanForward (bK board))

wPawnMoves :: Word64 -> Word64 -> Word64 -> [Move] -> [Move]
wPawnMoves !pawns !emptySquares !opponentPieces =
    flip (foldBits $ mkMove 9) eastCaptureTargets
  . flip (foldBits $ mkMove 7) westCaptureTargets
  . flip (foldBits $ mkMove 8) singlePushTargets
  . flip (foldBits $ mkMove 16) doublePushTargets
 where
  doublePushTargets = shiftN singlePushTargets .&. emptySquares .&. rank4
  singlePushTargets = shiftN pawns .&. emptySquares
  eastCaptureTargets = shiftNE pawns .&. opponentPieces
  westCaptureTargets = shiftNW pawns .&. opponentPieces
  mkMove diff ms tsq
    | tsq >= 56 = (promoteTo m <$> [Queen, Rook, Bishop, Knight]) <> ms
    | otherwise = m : ms
   where m = move (tsq - diff) tsq

bPawnMoves :: Word64 -> Word64 -> Word64 -> [Move] -> [Move]
bPawnMoves !pawns !emptySquares !opponentPieces =
    flip (foldBits $ mkMove 9) westCaptureTargets
  . flip (foldBits $ mkMove 7) eastCaptureTargets
  . flip (foldBits $ mkMove 8) singlePushTargets
  . flip (foldBits $ mkMove 16) doublePushTargets
 where
  doublePushTargets = shiftS singlePushTargets .&. emptySquares .&. rank5
  singlePushTargets = shiftS pawns .&. emptySquares
  eastCaptureTargets = shiftSE pawns .&. opponentPieces
  westCaptureTargets = shiftSW pawns .&. opponentPieces
  mkMove diff ms tsq
    | tsq <= 7  = (promoteTo m <$> [Queen, Rook, Bishop, Knight]) <> ms
    | otherwise = m : ms
   where m = move (tsq + diff) tsq

slideMoves :: PieceType -> Position -> Word64 -> Word64 -> Word64 -> [Move] -> [Move]
slideMoves piece (Position bb c _ _ _) !ours !notOurs !occ =
  flip (foldBits gen) pieces
 where
  gen ms from = foldBits (mkMove from) ms (targets from)
  mkMove from ms to = move from to : ms
  targets sq = case piece of
    Rook -> rookTargets sq occ .&. notOurs
    Bishop -> bishopTargets sq occ .&. notOurs
    Queen -> queenTargets sq occ .&. notOurs
    _ -> error "Not a sliding piece"
  pieces = case (c, piece) of
    (White, Bishop) -> wB bb
    (Black, Bishop) -> bB bb
    (White, Rook)   -> wR bb
    (Black, Rook)   -> bR bb
    (White, Queen)  -> wQ bb
    (Black, Queen)  -> bQ bb
    _ -> 0

data Castle = Kingside | Queenside deriving (Eq, Ix, Ord, Show)

castlingRights :: Position -> [(Color, Castle)]
castlingRights Position{flags} = wks . wqs . bks . bqs $ [] where
  wks xs | flags `testMask` crwKs = (White, Kingside):xs
         | otherwise              = xs
  wqs xs | flags `testMask` crwQs = (White, Queenside):xs
         | otherwise              = xs
  bks xs | flags `testMask` crbKs = (Black, Kingside):xs
         | otherwise              = xs
  bqs xs | flags `testMask` crbQs = (Black, Queenside):xs
         | otherwise              = xs

canCastleKingside, canCastleQueenside :: Position -> Bool
canCastleKingside !pos@Position{board} = canCastleKingside' pos (occupied board)
canCastleQueenside !pos@Position{board} = canCastleQueenside' pos (occupied board)

canCastleKingside', canCastleQueenside' :: Position -> Word64 -> Bool
canCastleKingside' Position{board, color = White, flags} !occ =
  flags `testMask` crwKs && occ .&. crwKe == 0 &&
  not (any (attackedBy Black board occ) [E1, F1, G1])
canCastleKingside' Position{board, color = Black, flags} !occ = 
  flags `testMask` crbKs && occ .&. crbKe == 0 &&
  not (any (attackedBy White board occ) [E8, F8, G8])
canCastleQueenside' Position{board, color = White, flags} !occ =
  flags `testMask` crwQs && occ .&. crwQe == 0 &&
  not (any (attackedBy Black board occ) [E1, D1, C1])
canCastleQueenside' Position{board, color = Black, flags} !occ =
  flags `testMask` crbQs && occ .&. crbQe == 0 &&
  not (any (attackedBy White board occ) [E8, D8, C8])

wKscm, wQscm, bKscm, bQscm :: Move
wKscm = move E1 G1
wQscm = move E1 C1
bKscm = move E8 G8
bQscm = move E8 C8

attackedBy :: IsSquare sq => Color -> BB -> Word64 -> sq -> Bool
attackedBy White !bb@BB{wP, wN, wB, wR, wQ, wK} !occ (toIndex -> sq)
  | (wPawnAttacks ! sq) .&. wP /= 0 = True
  | (knightAttacks ! sq) .&. wN /= 0 = True
  | bishopTargets sq occ .&. wB /= 0 = True
  | rookTargets sq occ .&.   wR /= 0 = True
  | queenTargets sq occ .&.  wQ /= 0 = True
  | (kingAttacks ! sq) .&. wK /= 0   = True
  | otherwise                        = False
attackedBy Black !bb@BB{bP, bN, bB, bR, bQ, bK} !occ (toIndex -> sq)
  | (bPawnAttacks ! sq) .&. bP /= 0 = True
  | (knightAttacks ! sq) .&. bN /= 0 = True
  | bishopTargets sq occ .&. bB /= 0 = True
  | rookTargets sq occ .&.   bR /= 0 = True
  | queenTargets sq occ .&.  bQ /= 0 = True
  | (kingAttacks ! sq) .&. bK /= 0   = True
  | otherwise                        = False

notAFile, notABFile, notGHFile, notHFile, rank1, rank2, rank3, rank4, rank5, rank6, rank7, rank8 :: Word64
notAFile = 0xfefefefefefefefe
notABFile = 0xfcfcfcfcfcfcfcfc
notGHFile = 0x3f3f3f3f3f3f3f3f
notHFile = 0x7f7f7f7f7f7f7f7f
rank1 = 0x00000000000000ff
rank2 = 0x000000000000ff00
rank3 = 0x0000000000ff0000
rank4 = 0x00000000ff000000
rank5 = 0x000000ff00000000
rank6 = 0x0000ff0000000000
rank7 = 0x00ff000000000000
rank8 = 0xff00000000000000

epMask, crMask, crwKs, crwQs, crwKe, crwQe, crbKs, crbQs, crbKe, crbQe :: Word64
epMask = rank3 .|. rank6        -- mask for en passant
crMask = 0x9100000000000091     -- mask for castle rights
crwKs  = 0x0000000000000090     -- white: king & rook position for kingside castle
crwQs  = 0x0000000000000011     -- white: king & rook pisition for queenside castle^M
crwKe  = 0x0000000000000060     -- white: empty fields for kingside castle
crwQe  = 0x000000000000000e     -- white: empty fields for queenside castle
crbKs  = 0x9000000000000000     -- black: king & rook position for kingside castle
crbQs  = 0x1100000000000000     -- black: king & rook position for queenside castle^M
crbKe  = 0x6000000000000000     -- black: empty fields for kingside castle
crbQe  = 0x0e00000000000000     -- black: empty fields for queenside castle

kingAttacks, knightAttacks, wPawnAttacks, bPawnAttacks :: Vector Word64
kingAttacks = Vector.generate 64 $ \sq -> let b = bit sq in
  shiftN b .|. shiftNE b .|. shiftE b .|. shiftSE b .|.
  shiftS b .|. shiftSW b .|. shiftW b .|. shiftNW b
knightAttacks = Vector.generate 64 $ \sq -> let b = bit sq in
  shiftNNE b .|. shiftENE b .|.
  shiftESE b .|. shiftSSE b .|.
  shiftSSW b .|. shiftWSW b .|.
  shiftWNW b .|. shiftNNW b
wPawnAttacks = Vector.generate 64 $ \sq -> let b = bit sq in
  shiftSE b .|. shiftSW b
bPawnAttacks = Vector.generate 64 $ \sq -> let b = bit sq in
  shiftNE b .|. shiftNW b

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Eq, Show)

rookTargets, bishopTargets, queenTargets :: Int -> Word64 -> Word64
rookTargets sq occ = getRayTargets sq N occ .|. getRayTargets sq E occ
                 .|. getRayTargets sq S occ .|. getRayTargets sq W occ
bishopTargets sq occ = getRayTargets sq NW occ .|. getRayTargets sq NE occ
                   .|. getRayTargets sq SE occ .|. getRayTargets sq SW occ
queenTargets sq occ = rookTargets sq occ .|. bishopTargets sq occ

getRayTargets :: Int -> Direction -> Word64 -> Word64
getRayTargets sq dir occ = blocked $ attacks .&. occ where
  blocked 0 = attacks
  blocked bb = attacks `xor` (ray ! bitScan bb)
  attacks = ray ! sq
  (bitScan, ray) = case dir of
    NW -> (bitScanForward, attackNW)
    N  -> (bitScanForward, attackN)
    NE -> (bitScanForward, attackNE)
    E  -> (bitScanForward, attackE)
    SE -> (bitScanReverse, attackSE)
    S  -> (bitScanReverse, attackS)
    SW -> (bitScanReverse, attackSW)
    W  -> (bitScanReverse, attackW)

attackDir :: (Word64 -> Word64) -> Vector Word64
attackDir s = Vector.generate 64 $ \sq ->
  foldr (.|.) 0 $ take 7 $ tail $ iterate s (bit sq)

attackNW, attackN, attackNE, attackE, attackSE, attackS, attackSW, attackW :: Vector Word64
attackNW = attackDir shiftNW
attackN  = attackDir shiftN
attackNE = attackDir shiftNE
attackE  = attackDir shiftE
attackSE = attackDir shiftSE
attackS  = attackDir shiftS
attackSW = attackDir shiftSW
attackW  = attackDir shiftW

clearMask :: Bits a => a -> a -> a
clearMask a b = a .&. complement b

testMask :: Bits a => a -> a -> Bool
testMask a b = a .&. b == b

{-# INLINE clearMask #-}
{-# INLINE testMask #-}
{-# INLINE attackedBy #-}
{-# INLINE slideMoves #-}
{-# INLINE wPawnMoves #-}
{-# INLINE bPawnMoves #-}
{-# INLINE unpack #-}
{-# INLINE foldBits #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser Strict.ByteString Move #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser Lazy.ByteString Move #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser Strict.Text Move #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser Lazy.Text Move #-}
{-# SPECIALISE relaxedSAN :: Position -> Parser String Move #-}
{-# SPECIALISE strictSAN :: Position -> Parser Strict.ByteString Move #-}
{-# SPECIALISE strictSAN :: Position -> Parser Lazy.ByteString Move #-}
{-# SPECIALISE strictSAN :: Position -> Parser Strict.Text Move #-}
{-# SPECIALISE strictSAN :: Position -> Parser Lazy.Text Move #-}
{-# SPECIALISE strictSAN :: Position -> Parser String Move #-}
