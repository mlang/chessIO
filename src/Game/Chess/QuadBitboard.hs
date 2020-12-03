module Game.Chess.QuadBitboard (
  -- * The QuadBitboard data type
  QuadBitboard(..)
, white, occupied, pnr
, pawns, knights, bishops, rooks, queens, kings
, wPawns, wKnights, wBishops, wRooks, wQueens, wKings
, bPawns, bKnights, bBishops, bRooks, bQueens, bKings
, insufficientMaterial
, toString
  -- * Square codes
, Word4(..)
, pattern NoPiece
, pattern WhitePawn, pattern WhiteKnight, pattern WhiteBishop
, pattern WhiteRook, pattern WhiteQueen, pattern WhiteKing
, pattern BlackPawn, pattern BlackKnight, pattern BlackBishop
, pattern BlackRook, pattern BlackQueen, pattern BlackKing
  -- * Construction
, empty, standard, square
  -- * Access
, (!), setNibble
  -- * Transformations
  -- ** Normal moves
, move, move'
  -- ** Castling
, whiteKingsideCastle, whiteQueensideCastle
, blackKingsideCastle, blackQueensideCastle
  -- ** En passant
, enPassant
  -- ** Promotion
, whitePromotion, blackPromotion, whitePromotion', blackPromotion'
) where

import Control.Applicative (liftA2)
import Data.Binary ( Word8, Word64, Binary(put, get) )
import Data.Bits
    ( Bits(xor, complement, bit, unsafeShiftR, (.&.), unsafeShiftL,
           (.|.), testBit, setBit, clearBit, popCount),
      FiniteBits(..) )
import Data.Char (ord, toLower)
import Data.Ix ( Ix(inRange) )
import Data.List (groupBy, intercalate)
import Data.String (IsString(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed (Vector, MVector, Unbox)
import Foreign.Storable
import GHC.Enum
    ( boundedEnumFrom,
      boundedEnumFromThen,
      predError,
      succError,
      toEnumError )
import GHC.Ptr (castPtr, plusPtr)
import Numeric (showHex)

data QuadBitboard = QBB { black :: {-# UNPACK #-} !Word64
                        , pbq :: {-# UNPACK #-} !Word64
                        , nbk :: {-# UNPACK #-} !Word64
                        , rqk :: {-# UNPACK #-} !Word64
                        } deriving (Eq)

insufficientMaterial :: QuadBitboard -> Bool
insufficientMaterial qbb@QBB{black, pbq, nbk, rqk} =
  noPawnsNorQueens && eachSideHasOneKing && noRooks &&
  (oneSideHasAtMostOneMinorPiece || opposingBishopsOnEquallyColoredSquares)
 where
  eachSideHasOneKing = popCount (wKings qbb) == 1 && popCount (bKings qbb) == 1
  noPawnsNorQueens = pbq `xor` bishops qbb == 0
  noRooks = popCount rqk == 2
  oneSideHasAtMostOneMinorPiece =
    (popCount (nbk .&. complement black) == 1 && atMostOneMinorPiece black) ||
    (popCount (nbk .&. black) == 1 && atMostOneMinorPiece (complement black))
  opposingBishopsOnEquallyColoredSquares =
    popCount (knights qbb) == 0 &&
    popCount (nbk .&. black) == 2 && popCount (nbk .&. complement black) == 2 &&
    even (countTrailingZeros (wBishops qbb)) ==
    even (countTrailingZeros (bBishops qbb))
  atMostOneMinorPiece mask = popCount (nbk .&. mask) `elem` [1,2]

instance Storable QuadBitboard where
  sizeOf _ = 32
  alignment _ = 8
  peek p = QBB <$> peek (castPtr p) <*> peek (castPtr $ p `plusPtr` 8) <*> peek (castPtr $ p `plusPtr` 16) <*> peek (castPtr $ p `plusPtr` 24)
  poke p QBB{black, pbq, nbk, rqk} = do
    poke (castPtr $ p) black
    poke (castPtr $ p `plusPtr` 8) pbq
    poke (castPtr $ p `plusPtr` 16) nbk
    poke (castPtr $ p `plusPtr` 24) rqk

newtype instance MVector s QuadBitboard = MV_QuadBitboard (MVector s (Word64, Word64, Word64, Word64))
newtype instance Vector    QuadBitboard = V_QuadBitboard (Vector (Word64, Word64, Word64, Word64))
instance Unbox QuadBitboard

instance M.MVector MVector QuadBitboard where
  basicLength (MV_QuadBitboard v) = M.basicLength v
  basicUnsafeSlice i n (MV_QuadBitboard v) = MV_QuadBitboard $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_QuadBitboard v1) (MV_QuadBitboard v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_QuadBitboard <$> M.basicUnsafeNew n
  basicInitialize (MV_QuadBitboard v) = M.basicInitialize v
  basicUnsafeReplicate n (QBB b0 b1 b2 b3) = MV_QuadBitboard <$> M.basicUnsafeReplicate n (b0, b1, b2, b3)
  basicUnsafeRead (MV_QuadBitboard v) i = f <$> M.basicUnsafeRead v i where
    f (b0, b1, b2, b3) = QBB b0 b1 b2 b3
  basicUnsafeWrite (MV_QuadBitboard v) i (QBB b0 b1 b2 b3) = M.basicUnsafeWrite v i (b0, b1, b2, b3)
  basicClear (MV_QuadBitboard v) = M.basicClear v
  basicSet (MV_QuadBitboard v) (QBB b0 b1 b2 b3) = M.basicSet v (b0, b1, b2, b3)
  basicUnsafeCopy (MV_QuadBitboard v1) (MV_QuadBitboard v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_QuadBitboard v1) (MV_QuadBitboard v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_QuadBitboard v) n = MV_QuadBitboard <$> M.basicUnsafeGrow v n

instance G.Vector Vector QuadBitboard where
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (MV_QuadBitboard v) = V_QuadBitboard <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_QuadBitboard v) = MV_QuadBitboard <$> G.basicUnsafeThaw v
  basicLength (V_QuadBitboard v) = G.basicLength v
  basicUnsafeSlice i n (V_QuadBitboard v) = V_QuadBitboard $ G.basicUnsafeSlice  i n v
  basicUnsafeIndexM (V_QuadBitboard v) i
    = f <$> G.basicUnsafeIndexM v i where
    f (b0, b1, b2, b3) = QBB b0 b1 b2 b3
  basicUnsafeCopy (MV_QuadBitboard mv) (V_QuadBitboard v) = G.basicUnsafeCopy mv v
  elemseq _ (QBB b0 b1 b2 b3) z
    = G.elemseq (undefined :: Vector a) b0
    $ G.elemseq (undefined :: Vector a) b1
    $ G.elemseq (undefined :: Vector a) b2
    $ G.elemseq (undefined :: Vector a) b3
    z

occupied, pnr, white :: QuadBitboard -> Word64
occupied QBB{pbq, nbk, rqk} = pbq  .|.  nbk  .|.  rqk
pnr      QBB{pbq, nbk, rqk} = pbq `xor` nbk `xor` rqk
white                       = liftA2 xor occupied black

pawns, knights, bishops, rooks, queens, kings :: QuadBitboard -> Word64
pawns   = liftA2 (.&.) pnr pbq
knights = liftA2 (.&.) pnr nbk
bishops = liftA2 (.&.) pbq nbk
rooks   = liftA2 (.&.) pnr rqk
queens  = liftA2 (.&.) pbq rqk
kings   = liftA2 (.&.) nbk rqk

wPawns, wKnights, wBishops, wRooks, wQueens, wKings :: QuadBitboard -> Word64
wPawns   = liftA2 (.&.) pawns (complement . black)
wKnights = liftA2 (.&.) knights (complement . black)
wBishops = liftA2 (.&.) bishops (complement . black)
wRooks   = liftA2 (.&.) rooks (complement . black)
wQueens  = liftA2 (.&.) queens (complement . black)
wKings   = liftA2 (.&.) kings (complement . black)

bPawns, bKnights, bBishops, bRooks, bQueens, bKings :: QuadBitboard -> Word64
bPawns   = liftA2 (.&.) pawns black
bKnights = liftA2 (.&.) knights black
bBishops = liftA2 (.&.) bishops black
bRooks   = liftA2 (.&.) rooks black
bQueens  = liftA2 (.&.) queens black
bKings   = liftA2 (.&.) kings black

{-# INLINE pnr #-}
{-# INLINE occupied #-}
{-# INLINE white #-}
{-# INLINE pawns #-}
{-# INLINE knights #-}
{-# INLINE bishops #-}
{-# INLINE rooks #-}
{-# INLINE queens #-}
{-# INLINE kings #-}
{-# INLINE wPawns #-}
{-# INLINE wKnights #-}
{-# INLINE wBishops #-}
{-# INLINE wRooks #-}
{-# INLINE wQueens #-}
{-# INLINE wKings #-}
{-# INLINE bPawns #-}
{-# INLINE bKnights #-}
{-# INLINE bBishops #-}
{-# INLINE bRooks #-}
{-# INLINE bQueens #-}
{-# INLINE bKings #-}

empty, standard :: QuadBitboard
empty = QBB 0 0 0 0
standard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

newtype Word4 = W4 Word8
              deriving (Bits, Eq, Integral, Ix, Num, Ord, Read, Real, Show)

instance Bounded Word4 where
  minBound = 0
  maxBound = 0xF

instance Enum Word4 where
  succ x | x /= maxBound = x + 1
         | otherwise     = succError "Word4"
  pred x | x /= minBound = x - 1
         | otherwise     = predError "Word4"
  toEnum i | i >= 0 && i <= fromIntegral (maxBound::Word4) = W4 $ fromIntegral i
           | otherwise     = toEnumError "Word4" i (minBound::Word4, maxBound::Word4)
  fromEnum (W4 x) = fromIntegral x
  enumFrom        = boundedEnumFrom
  enumFromThen    = boundedEnumFromThen

instance FiniteBits Word4 where
  finiteBitSize _ = 4
  countLeadingZeros (W4 x) = countLeadingZeros x
  countTrailingZeros (W4 x) = countTrailingZeros x

pattern NoPiece :: Word4
pattern NoPiece     = 0

pattern WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing
  :: Word4
pattern WhitePawn   = 2
pattern WhiteKnight = 4
pattern WhiteBishop = 6
pattern WhiteRook   = 8
pattern WhiteQueen  = 10
pattern WhiteKing   = 12

pattern BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing
  :: Word4
pattern BlackPawn   = 3
pattern BlackKnight = 5
pattern BlackBishop = 7
pattern BlackRook   = 9
pattern BlackQueen  = 11
pattern BlackKing   = 13

-- | law: square i x ! i = x where inRange (0,63) i && inRange (0,15) x
{-# INLINE square #-}
square :: Int -> Word4 -> QuadBitboard
square !sq !nb = QBB (f 0) (f 1) (f 2) (f 3) where
  !b = bit sq
  f !n = fromIntegral ((nb `unsafeShiftR` n) .&. 1) * b

(!) :: QuadBitboard -> Int -> Word4
(!) QBB{..} sq = fromIntegral $ f black 0 .|. f pbq 1 .|. f nbk 2 .|. f rqk 3 where
  f !bb !n = ((bb `unsafeShiftR` sq) .&. 1) `unsafeShiftL` n

setNibble :: Bits nibble => QuadBitboard -> Int -> nibble -> QuadBitboard
setNibble QBB{..} sq nb = QBB (f 0 black) (f 1 pbq) (f 2 nbk) (f 3 rqk) where
  f n | nb `testBit` n = (`setBit` sq)
      | otherwise      = (`clearBit` sq)

instance Binary QuadBitboard where
  get = QBB <$> get <*> get <*> get <*> get
  put QBB{..} = put black *> put pbq *> put nbk *> put rqk

instance IsString QuadBitboard where
  fromString = go (7, 0) mempty where
    go _ !qbb "" = qbb
    go (!r,_) qbb ('/':xs) = go (r - 1, 0) qbb xs
    go (!r,!f) !qbb (x:xs)
      | inRange ('1','8') x = go (r, f + (ord x - ord '0')) qbb xs
      | otherwise = go (r, f + 1) (qbb <> square (r*8+f) nb) xs where
        nb = case x of
          'P' -> WhitePawn
          'N' -> WhiteKnight
          'B' -> WhiteBishop
          'R' -> WhiteRook
          'Q' -> WhiteQueen
          'K' -> WhiteKing
          'p' -> BlackPawn
          'n' -> BlackKnight
          'b' -> BlackBishop
          'r' -> BlackRook
          'q' -> BlackQueen
          'k' -> BlackKing
          _ -> error $ "QuadBitBoard.fromString: Illegal FEN character " <> show x

-- | bitwise XOR
instance Semigroup QuadBitboard where
  {-# INLINE (<>) #-}
  QBB b0 b1 b2 b3 <> QBB b0' b1' b2' b3' =
    QBB (b0 `xor` b0') (b1 `xor` b1') (b2 `xor` b2') (b3 `xor` b3')

instance Monoid QuadBitboard where
  mempty = empty

instance Show QuadBitboard where
  show QBB{..} =
     "QBB {black = 0x" <> showHex black
    (", pbq = 0x" <> showHex pbq
    (", nbk = 0x" <> showHex nbk
    (", rqk = 0x" <> showHex rqk "}")))

toString :: QuadBitboard -> String
toString qbb = intercalate "/" $ rank <$> [7, 6..0] where
  rank r = concatMap countEmpty . groupBy spaces $ charAt r <$> [0..7]
  countEmpty xs | head xs == spc = show $ length xs
                | otherwise      = xs
  spaces x y = x == y && x == spc
  charAt r f = maybe spc (if odd nb then toLower else id) $
    lookup (nb `div` 2) $ zip [1..] "PNBRQK"
   where nb = qbb ! (r*8+f)
  spc = ' '

-- | Move a nibble.  Note that this function, while convenient, isn't very
-- fast as it needs to lookup the source nibble value.
move :: QuadBitboard -> Int -> Int -> QuadBitboard
move qbb fromSq toSq = qbb <> move' fromSq (qbb ! fromSq) toSq (qbb ! toSq)

move' :: Int -> Word4 -> Int -> Word4 -> QuadBitboard
move' fromSq fromCode toSq toCode =
  square fromSq fromCode <> square toSq (fromCode `xor` toCode)

whiteKingsideCastle, whiteQueensideCastle, blackKingsideCastle, blackQueensideCastle
  :: QuadBitboard
whiteKingsideCastle  = move' 4 WhiteKing 6 NoPiece <> move' 7 WhiteRook 5 NoPiece
whiteQueensideCastle = move' 4 WhiteKing 2 NoPiece <> move' 0 WhiteRook 3 NoPiece
blackKingsideCastle  = move' 60 BlackKing 62 NoPiece <> move' 63 BlackRook 61 NoPiece
blackQueensideCastle = move' 60 BlackKing 58 NoPiece <> move' 56 BlackRook 59 NoPiece

enPassant :: Int -> Int -> QuadBitboard
enPassant fromSq toSq
  | fromSq < toSq
  = move' fromSq WhitePawn toSq NoPiece <> square (toSq-8) BlackPawn
  | otherwise
  = move' fromSq BlackPawn toSq NoPiece <> square (toSq+8) WhitePawn

whitePromotion, blackPromotion :: QuadBitboard -> Int -> Int -> Word4 -> QuadBitboard
whitePromotion qbb fromSq toSq promoCode =
  qbb <> whitePromotion' fromSq toSq (qbb ! toSq) promoCode
blackPromotion qbb fromSq toSq promoCode =
  qbb <> blackPromotion' fromSq toSq (qbb ! toSq) promoCode

whitePromotion', blackPromotion' :: Int -> Int -> Word4 -> Word4 -> QuadBitboard
whitePromotion' fromSq toSq toCode promoCode =
  square fromSq WhitePawn <> square toSq (toCode `xor` promoCode)
blackPromotion' fromSq toSq toCode promoCode =
  square fromSq BlackPawn <> square toSq (toCode `xor` promoCode)
