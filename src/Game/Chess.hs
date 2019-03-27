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
  Position, startpos
  -- ** Converting from/to Forsyth-Edwards-Notation
, fromFEN, toFEN
  -- * Chess moves
, Move
  -- ** Converting from/to algebraic notation
, fromSAN, fromUCI, toUCI
  -- ** Move generation
, moves
  -- ** Executing moves
, applyMove
) where

import Data.Bits
import Data.Char
import Data.Ix
import Data.List
import Data.Maybe
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import Data.Word
import Text.Megaparsec
import Text.Read

import Control.Applicative.Combinators
import Data.Functor (($>))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void String

data From = File Int
          | Rank Int
          | Square Int
          deriving (Show)

san :: Parser (PieceType, Maybe From, Bool, Int, Maybe PieceType, Maybe Char)
san = conv <$> piece
           <*> location
           <*> optional (optional (char '=') *> promo)
           <*> optional (char '+' <|> char '#') where
  conv pc (Nothing, Nothing, cap, to) = (pc, Nothing, cap, to,,)
  conv pc (Just f, Nothing, cap, to) = (pc, Just (File f), cap, to,,)
  conv pc (Nothing, Just r, cap, to) = (pc, Just (Rank r), cap, to,,)
  conv pc (Just f, Just r, cap, to) = (pc, Just (Square $ r*8+f), cap, to,,)
  piece = char 'N' $> Knight
      <|> char 'B' $> Bishop
      <|> char 'R' $> Rook
      <|> char 'Q' $> Queen
      <|> char 'K' $> King
      <|> pure Pawn
  location = try ((,,,) <$> (Just <$> file)
                        <*> pure Nothing
                        <*> capture
                        <*> square)
         <|> try ((,,,) <$> pure Nothing
                        <*> (Just <$> rank)
                        <*> capture
                        <*> square)
         <|> try ((,,,) <$> (Just <$> file)
                        <*> (Just <$> rank)
                        <*> capture
                        <*> square)
         <|>      (,,,) <$> pure Nothing
                        <*> pure Nothing
                        <*> capture
                        <*> square
  promo = char 'N' $> Knight
      <|> char 'B' $> Bishop
      <|> char 'R' $> Rook
      <|> char 'Q' $> Queen
  capture = option False $ char 'x' $> True
  square = frToInt <$> file <*> rank
  file = subtract (ord 'a') . ord <$> oneOf ['a'..'h']
  rank = subtract (ord '1') . ord <$> oneOf ['1'..'8']
  frToInt f r = r*8 + f

fromSAN :: Position -> String -> Either String Move
fromSAN pos s = case parse san "" s of
  Right (pc, from, capture, to, promo, status) ->
    case ms pc from to promo of
      [m] -> Right m
      [] -> Left "Illegal move"
      _ -> Left "Ambiguous move"
  Left err -> Left $ errorBundlePretty err
 where
  ms pc from to prm = filter (f from) $ moves pos where
   f (Just (Square from)) (unpack -> (from', to', prm')) =
     pAt pos from' == pc && from' == from && to' == to && prm' == prm
   f (Just (File ff)) (unpack -> (from', to', prm')) =
     pAt pos from' == pc && from' `mod` 8 == ff && to == to' && prm == prm'
   f (Just (Rank fr)) (unpack -> (from', to', prm')) =
     pAt pos from' == pc && from' `div` 8 == fr && to == to' && prm == prm'
   f Nothing (unpack -> (from', to', prm')) =
     pAt pos from' == pc && to == to' && prm == prm'
  pAt (Position BB{wP, wN, wB, wR, wQ, wK, bP, bN, bB, bR, bQ, bK} _ _ _ _) sq
    | (wP .|. bP) `testBit` sq = Pawn
    | (wN .|. bN) `testBit` sq = Knight
    | (wB .|. bB) `testBit` sq = Bishop
    | (wR .|. bR) `testBit` sq = Rook
    | (wQ .|. bQ) `testBit` sq = Queen
    | otherwise                = King

-- | The starting position as given by the FEN string
--   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".
startpos :: Position
startpos = fromJust $
  fromFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Color = White | Black deriving (Eq, Ord, Show)

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
        deriving (Bounded, Enum, Eq, Show)

data Castling = Kingside | Queenside deriving (Eq, Ord, Show)

data BB = BB { wP, wN, wB, wR, wQ, wK :: !Word64
             , bP, bN, bB, bR, bQ, bK :: !Word64
             } deriving (Eq, Show)

data Position = Position {
  board :: !BB
, color :: !Color
, flags :: !Word64
, halfMoveClock :: !Int
, moveNumber :: !Int
} deriving (Eq)

emptyBB :: BB
emptyBB = BB 0 0 0 0 0 0 0 0 0 0 0 0

-- | Construct a position from Forsyth-Edwards-Notation.
fromFEN :: String -> Maybe Position
fromFEN s
  | length parts /= 6
  = Nothing
  | otherwise =
    Position <$> readBoard (parts !! 0)
             <*> readColor (parts !! 1)
             <*> readFlags (parts !! 2) (parts !! 3)
             <*> readMaybe (parts !! 4)
             <*> readMaybe (parts !! 5)
 where
  parts = words s
  readBoard = go (sqToRF A8) emptyBB where
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

sqToRF :: Sq -> (Int, Int)
sqToRF sq = fromEnum sq `divMod` 8

rfBit :: Bits bits => (Int, Int) -> bits
rfBit (r,f) | inRange (0,7) r && inRange (0,7) f = bit $ r*8 + f
            | otherwise = error $ "Out of range: " <> show r <> " " <> show f

-- | Convert a position to Forsyth-Edwards-Notation.
toFEN :: Position -> String
toFEN (Position bb c flgs hm mn) = intercalate " " [
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
    (r, f) = bitScanForward x `divMod` 8
  rank r = concatMap countEmpty $ groupBy (\x y -> x == y && x == ' ') $
           charAt r <$> [0..7]
  countEmpty xs | head xs == ' ' = if length xs == 8 then "" else show (length xs)
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
foldBits f !a n = foldBits f (f a lsb) (n `clearBit` lsb) where
  !lsb = countTrailingZeros n

bitScanForward, bitScanReverse :: Word64 -> Int
bitScanForward = countTrailingZeros
bitScanReverse = (63 -) . countLeadingZeros

newtype Move = Move Word16 deriving (Eq)

move :: Int -> Int -> Move
move from to = Move $ fromIntegral from .|. fromIntegral to `unsafeShiftL` 6

promoteTo :: Move -> PieceType -> Move
promoteTo (Move x) = Move . set where
  set Knight = x .|. 0b001_000000_000000
  set Bishop = x .|. 0b010_000000_000000
  set Rook   = x .|. 0b011_000000_000000
  set Queen  = x .|. 0b100_000000_000000
  set _      = x

unpack :: Move -> (Int, Int, Maybe PieceType)
unpack (Move x) = ( fromIntegral (x .&. 0b111111)
                  , fromIntegral ((x `unsafeShiftR` 6) .&. 0b111111)
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
  | length from == 2 && length to == 2 && length promo == 0
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

-- | Convert a move to the format used by the Universal Chess Interface protocol.
toUCI :: Move -> String
toUCI (unpack -> (from, to, promo)) = coord from <> coord to <> p where
  coord x = let (r,f) = x `divMod` 8 in
            chr (f + (ord 'a')) : [chr (r + (ord '1'))]
  p = case promo of
    Just Queen -> "q"
    Just Rook -> "r"
    Just Bishop -> "b"
    Just Knight -> "n"
    _ -> ""

-- | Validate that a certain move is legal in the given position.
relativeTo :: Position -> Move -> Maybe Move
relativeTo pos m | m `elem` (moves pos) = Just m
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

applyMove :: Position -> Move -> Position
applyMove pos m@(unpack -> (from, to, promo))
  | m == wKscm && flags pos `testMask` crwKs
  = pos { board = bb { wK = wK bb `xor` mask
                     , wR = wR bb `xor` (bit (fromEnum H1) `setBit` (fromEnum F1))
                     }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (rank1 .|. epMask)
        }
  | m == wQscm && flags pos `testMask` crwQs
  = pos { board = bb { wK = wK bb `xor` mask
                     , wR = wR bb `xor` (bit (fromEnum A1) `setBit` (fromEnum D1))
                     }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (rank1 .|. epMask)
        }
  | m == bKscm && flags pos `testMask` crbKs
  = pos { board = bb { bK = bK bb `xor` mask
                     , bR = bR bb `xor` (bit (fromEnum H8) `setBit` (fromEnum F8))
                     }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (rank8 .|. epMask)
        }
  | m == bQscm && flags pos `testMask` crbQs
  = pos { board = bb { bK = bK bb `xor` mask
                     , bR = bR bb `xor` (bit (fromEnum A8) `setBit` (fromEnum D8))
                     }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (rank8 .|. epMask)
        }
  | Just Queen <- promo
  , color pos == White
  = pos { board = clearB { wP = wP bb `clearBit` from
                         , wQ = wQ bb `setBit` to
                         }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (epMask .|. bit to)
        }
  | Just Rook <- promo
  , color pos == White
  = pos { board = clearB { wP = wP bb `clearBit` from
                         , wR = wR bb `setBit` to
                         }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (epMask .|. bit to)
        }
  | Just Bishop <- promo
  , color pos == White
  = pos { board = clearB { wP = wP bb `clearBit` from
                         , wB = wB bb `setBit` to
                         }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (epMask .|. bit to)
        }
  | Just Knight <- promo
  , color pos == White
  = pos { board = clearB { wP = wP bb `clearBit` from
                         , wN = wN bb `setBit` to
                         }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (epMask .|. bit to)
        }
  | Just Queen <- promo
  , color pos == Black
  = pos { board = clearW { bP = bP bb `clearBit` from
                         , bQ = bQ bb `setBit` to
                         }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (epMask .|. bit to)
        }  
  | Just Rook <- promo
  , color pos == Black
  = pos { board = clearW { bP = bP bb `clearBit` from
                         , bR = bR bb `setBit` to
                         }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (epMask .|. bit to)
        }
  | Just Bishop <- promo
  , color pos == Black
  = pos { board = clearW { bP = bP bb `clearBit` from
                         , bB = bB bb `setBit` to
                         }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (epMask .|. bit to)
        }
  | Just Knight <- promo
  , color pos == Black
  = pos { board = clearW { bP = bP bb `clearBit` from
                         , bN = bN bb `setBit` to
                         }
        , color = opponent (color pos)
        , flags = flags pos `clearMask` (epMask .|. bit to)
        }
  | otherwise
  = pos { board = newBoard
        , color = opponent (color pos)
        , flags = (flags pos `clearMask` (epMask .|. mask)) .|. dpp }
 where
  !bb = board pos
  epBit = case color pos of
    White | bit from .&. wP bb /= 0 -> shiftS $ flags pos .&. rank6 .&. bit to
    Black | bit from .&. bP bb /= 0 -> shiftN $ flags pos .&. rank3 .&. bit to
    _ -> 0
  clearW = bb { wP = wP bb `clearBit` to `clearMask` epBit
              , wN = wN bb `clearBit` to
              , wB = wB bb `clearBit` to
              , wR = wR bb `clearBit` to
              , wQ = wQ bb `clearBit` to
              }
  clearB = bb { bP = bP bb `clearBit` to `clearMask` epBit
              , bN = bN bb `clearBit` to
              , bB = bB bb `clearBit` to
              , bR = bR bb `clearBit` to
              , bQ = bQ bb `clearBit` to
              }
  !mask = bit from `setBit` to
  newBoard = case color pos of
    White | wP bb `testBit` from -> clearB { wP = wP bb `xor` mask }
          | wN bb `testBit` from -> clearB { wN = wN bb `xor` mask }
          | wB bb `testBit` from -> clearB { wB = wB bb `xor` mask }
          | wR bb `testBit` from -> clearB { wR = wR bb `xor` mask }
          | wQ bb `testBit` from -> clearB { wQ = wQ bb `xor` mask }
          | otherwise -> clearB { wK = wK bb `xor` mask }
    Black | bP bb `testBit` from -> clearW { bP = bP bb `xor` mask }
          | bN bb `testBit` from -> clearW { bN = bN bb `xor` mask }
          | bB bb `testBit` from -> clearW { bB = bB bb `xor` mask }
          | bR bb `testBit` from -> clearW { bR = bR bb `xor` mask }
          | bQ bb `testBit` from -> clearW { bQ = bQ bb `xor` mask }
          | otherwise -> clearW { bK = bK bb `xor` mask }
  dpp = case color pos of
    White | bit from .&. rank2 .&. wP bb /= 0 && from + 16 == to -> bit (from + 8)
    Black | bit from .&. rank7 .&. bP bb /= 0 && from - 16 == to -> bit (from - 8)
    _                                                            -> 0

-- | Generate a list of possible moves for the given position.
moves :: Position -> [Move]
moves pos@Position{color} = filter (not . check) $
      pawnMoves pos
   <> slideMoves Bishop pos
   <> slideMoves Rook pos
   <> slideMoves Queen pos
   <> knightMoves pos
   <> kingMoves pos
 where
  check m = let board' = board (applyMove pos m) in case color of
    White -> let kSq = bitScanForward (wK board') in
             attackedBy Black kSq board'
    Black -> let kSq = bitScanForward (bK board') in
             attackedBy White kSq board'

pawnMoves :: Position -> [Move]
pawnMoves (Position bb White flags _ _) =
  wPawnMoves (wP bb) (notOccupied bb) (occupiedBy Black bb .|. (flags .&. epMask))
pawnMoves (Position bb Black flags _ _) =
  bPawnMoves (bP bb) (notOccupied bb) (occupiedBy White bb .|. (flags .&. epMask))

wPawnMoves :: Word64 -> Word64 -> Word64 -> [Move]
wPawnMoves pawns empty opponentPieces =
  foldBits (mkMove 9) (foldBits (mkMove 7) (foldBits (mkMove 8) (foldBits (mkMove 16) [] doublePushTargets) singlePushTargets) westCaptureTargets) eastCaptureTargets
 where
  doublePushTargets = shiftN singlePushTargets .&. empty .&. rank4
  singlePushTargets = shiftN pawns .&. empty
  eastCaptureTargets = shiftNE pawns .&. opponentPieces
  westCaptureTargets = shiftNW pawns .&. opponentPieces
  mkMove diff ms tsq
    | tsq >= 56 = (promoteTo m <$> [Queen, Rook, Bishop, Knight]) <> ms
    | otherwise = m : ms
   where m = move (tsq - diff) tsq

bPawnMoves :: Word64 -> Word64 -> Word64 -> [Move]
bPawnMoves pawns empty opponentPieces =
  foldBits (mkMove 9) (foldBits (mkMove 7) (foldBits (mkMove 8) (foldBits (mkMove 16) [] doublePushTargets) singlePushTargets) eastCaptureTargets) westCaptureTargets
 where
  doublePushTargets = shiftS singlePushTargets .&. empty .&. rank5
  singlePushTargets = shiftS pawns .&. empty
  eastCaptureTargets = shiftSE pawns .&. opponentPieces
  westCaptureTargets = shiftSW pawns .&. opponentPieces
  mkMove diff ms tsq
    | tsq <= 7  = (promoteTo m <$> [Queen, Rook, Bishop, Knight]) <> ms
    | otherwise = m : ms
   where m = move (tsq + diff) tsq

slideMoves :: PieceType -> Position -> [Move]
slideMoves piece (Position bb c _ _ _) =
  foldBits gen mempty pieces
 where
  gen ms from = foldBits (mkMove from) ms (targets from)
  mkMove from ms to = move from to : ms
  targets sq = case piece of
    Rook -> rookTargets sq occ .&. notOurs
    Bishop -> bishopTargets sq occ .&. notOurs
    Queen -> queenTargets sq occ .&. notOurs
    _ -> error "Not a sliding piece"
  ours = occupiedBy c bb
  notOurs = complement ours
  occ = ours .|. occupiedBy (opponent c) bb
  pieces = case (c, piece) of
    (White, Bishop) -> wB bb
    (Black, Bishop) -> bB bb
    (White, Rook)   -> wR bb
    (Black, Rook)   -> bR bb
    (White, Queen)  -> wQ bb
    (Black, Queen)  -> bQ bb
    _ -> 0

knightMoves, kingMoves :: Position -> [Move]
knightMoves Position{board, color} = case color of
  White -> nMoves (wN board)
  Black -> nMoves (bN board)
 where
  nMoves = foldBits gen mempty
  gen ms sq = foldBits (mkMove sq) ms ((knightAttacks ! sq) .&. notOurs)
  mkMove from ms to = move from to : ms
  notOurs = complement $ occupiedBy color board

kingMoves pos@Position{board, color} = case color of
  White -> kMoves (wK board) <> wCastleMoves pos
  Black -> kMoves (bK board) <> bCastleMoves pos
 where
  kMoves = foldBits gen mempty
  gen ms sq = foldBits (mkMove sq) ms ((kingAttacks ! sq) .&. notOurs)
  mkMove from ms to = move from to : ms
  notOurs = complement $ occupiedBy color board

wCastleMoves, bCastleMoves :: Position -> [Move]
wCastleMoves (Position board _ flags _ _) = short <> long where
  short | flags `testMask` crwKs && occupied board .&. crwKe == 0 &&
          not (attackedBy Black (fromEnum E1) board) &&
          not (attackedBy Black (fromEnum F1) board)
        = [wKscm]
        | otherwise = []
  long  | flags `testMask` crwQs && occupied board .&. crwQe == 0 &&
          not (attackedBy Black (fromEnum E1) board) &&
          not (attackedBy Black (fromEnum D1) board)
        = [wQscm]
        | otherwise = []
bCastleMoves (Position board _ flags _ _) = short <> long where
  short | flags `testMask` crbKs && occupied board .&. crbKe == 0 &&
          not (attackedBy White (fromEnum E8) board) &&
          not (attackedBy White (fromEnum F8) board)
        = [bKscm]
        | otherwise = []
  long  | flags `testMask` crbQs && occupied board .&. crbQe == 0 &&
          not (attackedBy White (fromEnum E8) board) &&
          not (attackedBy White (fromEnum D8) board)
        = [bQscm]
        | otherwise = []

wKscm, wQscm, bKscm, bQscm :: Move
wKscm = move (fromEnum E1) (fromEnum G1)
wQscm = move (fromEnum E1) (fromEnum C1)
bKscm = move (fromEnum E8) (fromEnum G8)
bQscm = move (fromEnum E8) (fromEnum C8)

attackedBy :: Color -> Int -> BB -> Bool
attackedBy White sq bb@BB{wP, wN, wB, wR, wQ, wK}
  | (wPawnAttacks ! sq) .&. wP /= 0 = True
  | (knightAttacks ! sq) .&. wN /= 0 = True
  | bishopTargets sq occ .&. wB /= 0 = True
  | rookTargets sq occ .&.   wR /= 0 = True
  | queenTargets sq occ .&.  wQ /= 0 = True
  | (kingAttacks ! sq) .&. wK /= 0   = True
  | otherwise                        = False
 where occ = occupied bb
attackedBy Black sq bb@BB{bP, bN, bB, bR, bQ, bK}
  | (bPawnAttacks ! sq) .&. bP /= 0 = True
  | (knightAttacks ! sq) .&. bN /= 0 = True
  | bishopTargets sq occ .&. bB /= 0 = True
  | rookTargets sq occ .&.   bR /= 0 = True
  | queenTargets sq occ .&.  bQ /= 0 = True
  | (kingAttacks ! sq) .&. bK /= 0   = True
  | otherwise                        = False
 where occ = occupied bb

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
{-# INLINE attackedBy #-}
{-# INLINE kingMoves #-}
{-# INLINE knightMoves #-}
{-# INLINE slideMoves #-}
{-# INLINE pawnMoves #-}
{-# INLINE wPawnMoves #-}
{-# INLINE bPawnMoves #-}
{-# INLINE unpack #-}
{-# INLINE foldBits #-}
