{-|
Module      : Game.Chess
Description : Basic data types and functions related to the game of chess
Copyright   : (c) Mario Lang, 2020
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
module Game.Chess.Internal where

import Control.Lens (view)
import Control.Lens.Iso (from)
import Data.Bits
  ( Bits((.&.), testBit, unsafeShiftR, unsafeShiftL, xor, (.|.), bit, complement),
    FiniteBits(countLeadingZeros, countTrailingZeros) )
import Data.Char ( ord, chr )
import Data.Hashable
import Data.Ix ( Ix(inRange) )
import Data.List (nub, sortOn)
import Data.Maybe ( fromJust, isJust, listToMaybe )
import Data.Ord (Down(..))
import Data.String ( IsString(..) )
import Data.Vector.Unboxed (Vector, MVector, (!), Unbox)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Word ( Word16, Word64 )
import Foreign.Storable
import Game.Chess.Internal.Square
import Game.Chess.Internal.QuadBitboard (QuadBitboard)
import qualified Game.Chess.Internal.QuadBitboard as QBB
import Text.Read (readMaybe)
import GHC.Stack (HasCallStack)

capturing :: Position -> Ply -> Maybe PieceType
capturing pos@Position{flags} (plyTarget -> to)
  | (flags .&. epMask) `testBit` unSq to = Just Pawn
  | otherwise = snd <$> pieceAt pos to

isCapture :: Position -> Ply -> Bool
isCapture Position{qbb, flags} =
  testBit (QBB.occupied qbb .|. (flags .&. epMask)) . unSq . plyTarget

-- | The starting position as given by the FEN string
--   "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".
startpos :: Position
startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

instance IsString Position where fromString = fromJust . fromFEN

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ix, Ord, Show)

data Color = Black | White deriving (Eq, Ix, Ord, Show)

instance Hashable Color where
  hashWithSalt s Black = s `hashWithSalt` (0 :: Int)
  hashWithSalt s White = s `hashWithSalt` (1 :: Int)

pieceAt :: Position -> Square -> Maybe (Color, PieceType)
pieceAt Position{qbb} (unSq -> sq) = case qbb QBB.! sq of
  QBB.WhitePawn -> Just (White, Pawn)
  QBB.WhiteKnight -> Just (White, Knight)
  QBB.WhiteBishop -> Just (White, Bishop)
  QBB.WhiteRook -> Just (White, Rook)
  QBB.WhiteQueen -> Just (White, Queen)
  QBB.WhiteKing -> Just (White, King)
  QBB.BlackPawn -> Just (Black, Pawn)
  QBB.BlackKnight -> Just (Black, Knight)
  QBB.BlackBishop -> Just (Black, Bishop)
  QBB.BlackRook -> Just (Black, Rook)
  QBB.BlackQueen -> Just (Black, Queen)
  QBB.BlackKing -> Just (Black, King)
  _             -> Nothing

opponent :: Color -> Color
opponent White = Black
opponent Black = White

data Piece = Piece !Color !PieceType deriving (Eq, Show)

data Position = Position {
  qbb :: {-# UNPACK #-} !QuadBitboard
, color :: !Color
  -- ^ active color
, flags :: {-# UNPACK #-} !Word64
, halfMoveClock :: {-# UNPACK #-} !Int
, moveNumber :: {-# UNPACK #-} !Int
  -- ^ number of the full move
}

-- Article 9.2 states that a position is considered
-- identical to another if the same player is on move, the same types of
-- pieces of the same colors occupy the same squares, and the same moves
-- are available to each player; in particular, each player has the same
-- castling and en passant capturing rights.
instance Eq Position where
  a == b = qbb a == qbb b && color a == color b && flags a == flags b

instance Hashable Position where
  hashWithSalt s Position{qbb, color, flags} =
    s `hashWithSalt` qbb `hashWithSalt` color `hashWithSalt` flags

repetitions :: [Position] -> Maybe (Int, Position)
repetitions p = listToMaybe . sortOn (Down . fst) . fmap f $ nub p where
  f x = (count x p, x)
  count x = length . filter (== x)

instance Show Position where
  show p = '"' : toFEN p <> ['"']

insufficientMaterial :: Position -> Bool
insufficientMaterial = QBB.insufficientMaterial . qbb

-- | Construct a position from Forsyth-Edwards-Notation.
fromFEN :: String -> Maybe Position
fromFEN fen
  | length parts /= 6
  = Nothing
  | otherwise =
    Position <$> Just (fromString (parts !! 0))
             <*> readColor (parts !! 1)
             <*> readFlags (parts !! 2) (parts !! 3)
             <*> readMaybe (parts !! 4)
             <*> readMaybe (parts !! 5)
 where
  parts = words fen
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

-- | Convert a position to Forsyth-Edwards-Notation.
toFEN :: Position -> String
toFEN Position{qbb, color, flags, halfMoveClock, moveNumber} = unwords
  [ QBB.toString qbb
  , showColor color
  , showCst (flags `clearMask` epMask)
  , showEP (flags .&. epMask)
  , show halfMoveClock
  , show moveNumber
  ]
 where
  showColor White = "w"
  showColor Black = "b"
  showCst x
    | str == "" = "-"
    | otherwise = str
   where 
    str = snd . wks . wqs . bks . bqs $ (x, "")
    wks (v, xs) | v `testMask` crwKs = (v, 'K':xs)
                | otherwise          = (v, xs)
    wqs (v, xs) | v `testMask` crwQs = (v, 'Q':xs)
                | otherwise          = (v, xs)
    bks (v, xs) | v `testMask` crbKs = (v, 'k':xs)
                | otherwise          = (v, xs)
    bqs (v, xs) | v `testMask` crbQs = (v, 'q':xs)
                | otherwise          = (v, xs)
  showEP 0 = "-"
  showEP x = toCoord . Sq . bitScanForward $ x

occupiedBy :: Color -> QuadBitboard -> Word64
occupiedBy White = QBB.white
occupiedBy Black = QBB.black

occupied :: QuadBitboard -> Word64
occupied = QBB.occupied

foldBits :: (a -> Int -> a) -> a -> Word64 -> a
foldBits f = go where
  go a 0 = a
  go a n = go (f a $ countTrailingZeros n) $! n .&. pred n

bitScanForward, bitScanReverse :: Word64 -> Int
bitScanForward = countTrailingZeros
bitScanReverse = (63 -) . countLeadingZeros

newtype Ply = Ply Word16 deriving (Eq, Storable)

instance Show Ply where
  show (unpack -> (from, to, promo)) = "move " <> show from <> " " <> show to <> p where
    p = case promo of
      Just piece -> " `promoteTo` " <> show piece
      Nothing -> ""

newtype instance MVector s Ply = MV_Ply (MVector s Word16)
newtype instance Vector    Ply = V_Ply (Vector Word16)

instance M.MVector MVector Ply where
  basicLength (MV_Ply v) = M.basicLength v
  basicUnsafeSlice i n (MV_Ply v) = MV_Ply $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Ply v1) (MV_Ply v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Ply <$> M.basicUnsafeNew n
  basicInitialize (MV_Ply v) = M.basicInitialize v
  basicUnsafeReplicate n (Ply pl) = MV_Ply <$> M.basicUnsafeReplicate n pl
  basicUnsafeRead (MV_Ply v) i = Ply <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Ply v) i (Ply pl) = M.basicUnsafeWrite v i pl
  basicClear (MV_Ply v) = M.basicClear v
  basicSet (MV_Ply v) (Ply pl) = M.basicSet v pl
  basicUnsafeCopy (MV_Ply v1) (MV_Ply v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Ply v1) (MV_Ply v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Ply v) n = MV_Ply <$> M.basicUnsafeGrow v n

instance G.Vector Vector Ply where
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_Ply v) = V_Ply <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Ply v) = MV_Ply <$> G.basicUnsafeThaw v
  basicLength (V_Ply v) = G.basicLength v
  basicUnsafeSlice i n (V_Ply v) = V_Ply $ G.basicUnsafeSlice  i n v
  basicUnsafeIndexM (V_Ply v) i = Ply <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Ply mv) (V_Ply v) = G.basicUnsafeCopy mv v
  elemseq _ pl z = G.elemseq (undefined :: Vector a) pl $ z

instance Unbox Ply

move :: Square -> Square -> Ply
move (unSq -> from) (unSq -> to) =
  Ply $ fromIntegral to .|. fromIntegral from `unsafeShiftL` 6

promoteTo :: Ply -> PieceType -> Ply
promoteTo (Ply x) = Ply . set where
  set Knight = x .&. 0xfff .|. 0x1000
  set Bishop = x .&. 0xfff .|. 0x2000
  set Rook   = x .&. 0xfff .|. 0x3000
  set Queen  = x .&. 0xfff .|. 0x4000
  set _      = x

plySource, plyTarget :: Ply -> Square
plySource (Ply x) = Sq $ fromIntegral ((x `unsafeShiftR` 6) .&. 0b111111)
plyTarget (Ply x) = Sq $ fromIntegral (x .&. 0b111111)

plyPromotion :: Ply -> Maybe PieceType
plyPromotion (Ply x) = case x `unsafeShiftR` 12 of
  1 -> Just Knight
  2 -> Just Bishop
  3 -> Just Rook
  4 -> Just Queen
  _ -> Nothing

unpack :: Ply -> (Square, Square, Maybe PieceType)
unpack (Ply x) = ( Sq $ fromIntegral ((x `unsafeShiftR` 6) .&. 0b111111)
                 , Sq $ fromIntegral (x .&. 0b111111)
                 , piece)
 where
  !piece = case x `unsafeShiftR` 12 of
    1 -> Just Knight
    2 -> Just Bishop
    3 -> Just Rook
    4 -> Just Queen
    _ -> Nothing

fromPolyglot :: Position -> Ply -> Ply
fromPolyglot pos pl@(unpack -> (from, to, _)) = case color pos of
  White | from == E1
        , canCastleKingside pos
        , to == H1
        -> wKscm
        | from == E1
        , canCastleQueenside pos
        , to == A1
        -> wQscm
  Black | from == E8
        , canCastleKingside pos
        , to == H8
        -> bKscm
        | from == E8
        , canCastleQueenside pos
        , to == A8
        -> bQscm
  _ -> pl

toPolyglot :: Position -> Ply -> Ply
toPolyglot pos pl@(unpack -> (from, to, _)) = case color pos of
  White | from == E1
        , canCastleKingside pos
        , to == G1
        -> from `move` H1
        | from == E1
        , canCastleQueenside pos
        , to == C1
        -> from `move` A1
  Black | from == E8
        , canCastleKingside pos
        , to == G8
        -> from `move` H8
        | from == E8
        , canCastleQueenside pos
        , to == C8
        -> from `move` A8
  _ -> pl

-- | Parse a move in the format used by the Universal Chess Interface protocol.
fromUCI :: Position -> String -> Maybe Ply
fromUCI pos (fmap (splitAt 2) . splitAt 2 -> (src, (dst, promo)))
  | null promo
  = move <$> readCoord src <*> readCoord dst >>= relativeTo pos
  | otherwise
  = (\f t p -> move f t `promoteTo` p) <$> readCoord src
                                       <*> readCoord dst
                                       <*> readPromo promo
      >>= relativeTo pos
 where
  readCoord [f,r]
    | inRange ('a','h') f && inRange ('1','8') r
    = Just . view (from rankFile) $ (mkRank $ ord r - ord '1',  mkFile $ ord f - ord 'a')
  readCoord _ = Nothing
  readPromo "q" = Just Queen
  readPromo "r" = Just Rook
  readPromo "b" = Just Bishop
  readPromo "n" = Just Knight
  readPromo _ = Nothing

-- | Convert a move to the format used by the Universal Chess Interface protocol.
toUCI :: Ply -> String
toUCI (unpack -> (from, to, promo)) = coord from <> coord to <> p where
  coord x = let (r,f) = view rankFile x in
            chr (unFile f + ord 'a') : [chr (unRank r + ord '1')]
  p = case promo of
    Just Queen -> "q"
    Just Rook -> "r"
    Just Bishop -> "b"
    Just Knight -> "n"
    _ -> ""

-- | Validate that a certain move is legal in the given position.
relativeTo :: Position -> Ply -> Maybe Ply
relativeTo pos m | m `elem` legalPlies pos = Just m
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
-- if it isn't.  See 'unsafeDoPly' for a version that omits the legality check.
doPly :: HasCallStack => Position -> Ply -> Position
doPly p m
  | m `elem` legalPlies p = unsafeDoPly p m
  | otherwise        = error "Game.Chess.doPly: Illegal move"

-- | An unsafe version of 'doPly'.  Only use this if you are sure the given move
-- can be applied to the position.  This is useful if the move has been generated
-- by the 'legalPlies' function.
unsafeDoPly :: Position -> Ply -> Position
unsafeDoPly pos@Position{color, halfMoveClock, moveNumber} m =
  pos' { color = opponent color
       , halfMoveClock = if isCapture pos m || isPawnPush pos m
                         then 0
                         else succ halfMoveClock
       , moveNumber = if color == Black
                      then succ moveNumber
                      else moveNumber
       }
 where
  pos' = unsafeDoPly' pos m
  isPawnPush p m = case pieceAt p (plySource m) of
    Just (_, Pawn) -> True
    _         -> False

unsafeDoPly' :: Position -> Ply -> Position
unsafeDoPly' pos@Position{qbb, flags} m@(unpack -> (from, to, promo))
  | m == wKscm && flags `testMask` crwKs
  = pos { qbb = qbb <> QBB.whiteKingsideCastle
        , flags = flags `clearMask` (rank1 .|. epMask)
        }
  | m == wQscm && flags `testMask` crwQs
  = pos { qbb = qbb <> QBB.whiteQueensideCastle
        , flags = flags `clearMask` (rank1 .|. epMask)
        }
  | m == bKscm && flags `testMask` crbKs
  = pos { qbb = qbb <> QBB.blackKingsideCastle
        , flags = flags `clearMask` (rank8 .|. epMask)
        }
  | m == bQscm && flags `testMask` crbQs
  = pos { qbb = qbb <> QBB.blackQueensideCastle
        , flags = flags `clearMask` (rank8 .|. epMask)
        }
  | Just piece <- promo
  = case color pos of
      White -> case piece of
        Queen -> pos { qbb = QBB.whitePromotion qbb (unSq from) (unSq to) QBB.WhiteQueen
                     , flags = flags `clearMask` (epMask .|. bit (unSq to))
                     }
        Rook  -> pos { qbb = QBB.whitePromotion qbb (unSq from) (unSq to) QBB.WhiteRook
                     , flags = flags `clearMask` (epMask .|. bit (unSq to))
                     }
        Bishop -> pos { qbb = QBB.whitePromotion qbb (unSq from) (unSq to) QBB.WhiteBishop
                      , flags = flags `clearMask` (epMask .|. bit (unSq to))
                      }
        Knight -> pos { qbb = QBB.whitePromotion qbb (unSq from) (unSq to) QBB.WhiteKnight
                      , flags = flags `clearMask` (epMask .|. bit (unSq to))
                      }
        _ -> error "Impossible: White tried to promote to Pawn"
      Black -> case piece of
        Queen -> pos { qbb = QBB.blackPromotion qbb (unSq from) (unSq to) QBB.BlackQueen
                     , flags = flags `clearMask` (epMask .|. bit (unSq to))
                     }  
        Rook   -> pos { qbb = QBB.blackPromotion qbb (unSq from) (unSq to) QBB.BlackRook
                      , flags = flags `clearMask` (epMask .|. bit (unSq to))
                      }
        Bishop -> pos { qbb = QBB.blackPromotion qbb (unSq from) (unSq to) QBB.BlackBishop
                      , flags = flags `clearMask` (epMask .|. bit (unSq to))
                      }
        Knight -> pos { qbb = QBB.blackPromotion qbb (unSq from) (unSq to) QBB.BlackKnight
                      , flags = flags `clearMask` (epMask .|. bit (unSq to))
                      }
        _ -> error "Impossible: Black tried to promote to Pawn"
  | QBB.pawns qbb `testMask` fromMask &&
    toMask .&. (rank3 .|. rank6) .&. flags /= 0
  = pos { qbb = qbb <> QBB.enPassant (unSq from) (unSq to)
        , flags = flags `clearMask` toMask
        }
  | otherwise
  = pos { qbb = QBB.move qbb (unSq from) (unSq to)
        , flags = (flags `clearMask` (epMask .|. mask)) .|. dpp
        }
 where
  !fromMask = 1 `unsafeShiftL` (unSq from)
  !toMask = 1 `unsafeShiftL` (unSq to)
  !mask = fromMask .|. toMask
  dpp = case color pos of
    White | fromMask .&. rank2 .&. QBB.wPawns qbb /= 0 && unSq from + 16 == unSq to -> shiftN fromMask
    Black | fromMask .&. rank7 .&. QBB.bPawns qbb /= 0 && unSq from - 16 == unSq to -> shiftS fromMask
    _                                                            -> 0

-- | Generate a list of possible moves for the given position.
legalPlies :: Position -> [Ply]
legalPlies pos@Position{color, qbb, flags} = filter legalPly $
      kingMoves
    . knightMoves
    . slideMoves Queen pos notOurs occ
    . slideMoves Rook pos notOurs occ
    . slideMoves Bishop pos notOurs occ
    . pawnMoves
    $ []
 where
  legalPly = not . inCheck color . unsafeDoPly' pos
  !ours = occupiedBy color qbb
  !them = occupiedBy (opponent color) qbb
  !notOurs = complement ours
  !occ = ours .|. them
  (!pawnMoves, !knightMoves, !kingMoves) = case color of
    White ->
      ( wPawnMoves (QBB.wPawns qbb) (complement occ) (them .|. (flags .&. epMask))
      , flip (foldBits genNMoves) (QBB.wKnights qbb)
      , flip (foldBits genKMoves) (QBB.wKings qbb) . wShort . wLong)
    Black ->
      ( bPawnMoves (QBB.bPawns qbb) (complement occ) (them .|. (flags .&. epMask))
      , flip (foldBits genNMoves) (QBB.bKnights qbb)
      , flip (foldBits genKMoves) (QBB.bKings qbb) . bShort . bLong)
  genNMoves ms sq = foldBits (mkM sq) ms ((knightAttacks ! sq) .&. notOurs)
  genKMoves ms sq = foldBits (mkM sq) ms ((kingAttacks ! sq) .&. notOurs)
  wShort ml | canCastleKingside' pos occ = wKscm : ml
            | otherwise             = ml
  wLong ml  | canCastleQueenside' pos occ = wQscm : ml
            | otherwise                   = ml
  bShort ml | canCastleKingside' pos occ = bKscm : ml
            | otherwise                  = ml
  bLong ml  | canCastleQueenside' pos occ = bQscm : ml
            | otherwise              = ml
  mkM !from ms !to = move (Sq from) (Sq to) : ms

-- | Returns 'True' if 'Color' is in check in the given position.
inCheck :: Color -> Position -> Bool
inCheck White Position{qbb} =
  attackedBy Black qbb (occupied qbb) (Sq (bitScanForward (QBB.wKings qbb)))
inCheck Black Position{qbb} =
  attackedBy White qbb (occupied qbb) (Sq (bitScanForward (QBB.bKings qbb)))

wPawnMoves :: Word64 -> Word64 -> Word64 -> [Ply] -> [Ply]
wPawnMoves !pawns !emptySquares !opponentPieces =
    flip (foldBits $ mkPly 9) eastCaptureTargets
  . flip (foldBits $ mkPly 7) westCaptureTargets
  . flip (foldBits $ mkPly 8) singlePushTargets
  . flip (foldBits $ mkPly 16) doublePushTargets
 where
  doublePushTargets = shiftN singlePushTargets .&. emptySquares .&. rank4
  singlePushTargets = shiftN pawns .&. emptySquares
  eastCaptureTargets = shiftNE pawns .&. opponentPieces
  westCaptureTargets = shiftNW pawns .&. opponentPieces
  mkPly diff ms tsq
    | tsq >= 56 = (promoteTo m <$> [Queen, Rook, Bishop, Knight]) <> ms
    | otherwise = m : ms
   where m = move (Sq (tsq - diff)) (Sq tsq)

bPawnMoves :: Word64 -> Word64 -> Word64 -> [Ply] -> [Ply]
bPawnMoves !pawns !emptySquares !opponentPieces =
    flip (foldBits $ mkPly 9) westCaptureTargets
  . flip (foldBits $ mkPly 7) eastCaptureTargets
  . flip (foldBits $ mkPly 8) singlePushTargets
  . flip (foldBits $ mkPly 16) doublePushTargets
 where
  doublePushTargets = shiftS singlePushTargets .&. emptySquares .&. rank5
  singlePushTargets = shiftS pawns .&. emptySquares
  eastCaptureTargets = shiftSE pawns .&. opponentPieces
  westCaptureTargets = shiftSW pawns .&. opponentPieces
  mkPly diff ms tsq
    | tsq <= 7  = (promoteTo m <$> [Queen, Rook, Bishop, Knight]) <> ms
    | otherwise = m : ms
   where m = move (Sq (tsq + diff)) (Sq  tsq)

slideMoves :: PieceType -> Position -> Word64 -> Word64 -> [Ply] -> [Ply]
slideMoves piece (Position bb c _ _ _) !notOurs !occ =
  flip (foldBits gen) pieces
 where
  gen ms from = foldBits (mkPly from) ms (targets from)
  mkPly from ms to = move (Sq from) (Sq to) : ms
  targets sq = case piece of
    Rook -> rookTargets sq occ .&. notOurs
    Bishop -> bishopTargets sq occ .&. notOurs
    Queen -> queenTargets sq occ .&. notOurs
    _ -> error "Not a sliding piece"
  pieces = case (c, piece) of
    (White, Bishop) -> QBB.wBishops bb
    (Black, Bishop) -> QBB.bBishops bb
    (White, Rook)   -> QBB.wRooks bb
    (Black, Rook)   -> QBB.bRooks bb
    (White, Queen)  -> QBB.wQueens bb
    (Black, Queen)  -> QBB.bQueens bb
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

enPassantSquare :: Position -> Maybe Square
enPassantSquare Position{flags} = case flags .&. epMask of
  0 -> Nothing
  x -> Just . Sq . bitScanForward $ x

canCastleKingside, canCastleQueenside :: Position -> Bool
canCastleKingside pos@Position{qbb} = canCastleKingside' pos (occupied qbb)
canCastleQueenside pos@Position{qbb} = canCastleQueenside' pos (occupied qbb)

canCastleKingside', canCastleQueenside' :: Position -> Word64 -> Bool
canCastleKingside' Position{qbb, color = White, flags} !occ =
  flags `testMask` crwKs && occ .&. crwKe == 0 &&
  not (any (attackedBy Black qbb occ) [E1, F1, G1])
canCastleKingside' Position{qbb, color = Black, flags} !occ = 
  flags `testMask` crbKs && occ .&. crbKe == 0 &&
  not (any (attackedBy White qbb occ) [E8, F8, G8])
canCastleQueenside' Position{qbb, color = White, flags} !occ =
  flags `testMask` crwQs && occ .&. crwQe == 0 &&
  not (any (attackedBy Black qbb occ) [E1, D1, C1])
canCastleQueenside' Position{qbb, color = Black, flags} !occ =
  flags `testMask` crbQs && occ .&. crbQe == 0 &&
  not (any (attackedBy White qbb occ) [E8, D8, C8])

wKscm, wQscm, bKscm, bQscm :: Ply
wKscm = move E1 G1
wQscm = move E1 C1
bKscm = move E8 G8
bQscm = move E8 C8

attackedBy :: Color -> QuadBitboard -> Word64 -> Square -> Bool
attackedBy White qbb !occ (unSq -> sq)
  | (wPawnAttacks ! sq) .&. QBB.wPawns qbb /= 0 = True
  | (knightAttacks ! sq) .&. QBB.wKnights qbb /= 0 = True
  | bishopTargets sq occ .&. QBB.wBishops qbb /= 0 = True
  | rookTargets sq occ .&.   QBB.wRooks qbb /= 0 = True
  | queenTargets sq occ .&. QBB.wQueens qbb /= 0 = True
  | (kingAttacks ! sq) .&. QBB.wKings qbb /= 0   = True
  | otherwise                        = False
attackedBy Black qbb !occ (unSq -> sq)
  | (bPawnAttacks ! sq) .&. QBB.bPawns qbb /= 0 = True
  | (knightAttacks ! sq) .&. QBB.bKnights qbb /= 0 = True
  | bishopTargets sq occ .&. QBB.bBishops qbb /= 0 = True
  | rookTargets sq occ .&.   QBB.bRooks qbb /= 0 = True
  | queenTargets sq occ .&.  QBB.bQueens qbb /= 0 = True
  | (kingAttacks ! sq) .&. QBB.bKings qbb /= 0   = True
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

epMask, crwKs, crwQs, crwKe, crwQe, crbKs, crbQs, crbKe, crbQe :: Word64
epMask = rank3 .|. rank6        -- mask for en passant
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
rookTargets !sq !occ = getRayTargets sq N occ .|. getRayTargets sq E occ
                   .|. getRayTargets sq S occ .|. getRayTargets sq W occ
bishopTargets !sq !occ = getRayTargets sq NW occ .|. getRayTargets sq NE occ
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
{-# INLINE bitScanForward #-}
{-# INLINE bitScanReverse #-}
