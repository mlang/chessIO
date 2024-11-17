{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
module Game.Chess.Internal.QuadBitboard (
  -- * The QuadBitboard data type
  QuadBitboard
, occupied, black, white
, pawns, knights, bishops, rooks, queens, kings
, wPawns, wKnights, wBishops, wRooks, wQueens, wKings
, bPawns, bKnights, bBishops, bRooks, bQueens, bKings
, orthogonals, diagonals
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
, empty, standard
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

import           Control.Applicative         (liftA2)
import           Control.DeepSeq
import           Control.Lens                (view, (^.))
import           Control.Lens.Iso            (from)
import           Data.Binary                 (Binary)
import           Data.Bits                   (Bits (clearBit, complement, popCount, setBit, testBit, unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.)),
                                              FiniteBits (..))
import           Data.Char                   (ord, toLower)
import           Data.Hashable
import           Data.Ix                     (Ix (inRange))
import           Data.List                   (groupBy, intercalate)
import           Data.String                 (IsString (..))
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import           Data.Vector.Unboxed         (MVector, Unbox, Vector)
import           Data.Word                   (Word64, Word8)
import           Foreign.Storable
import           GHC.Enum                    (boundedEnumFrom,
                                              boundedEnumFromThen, predError,
                                              succError, toEnumError)
import           GHC.Exts                    (IsList (Item, fromList, toList))
import           GHC.Generics                (Generic)
import           GHC.Ptr                     (castPtr, plusPtr)
import           Game.Chess.Internal.Square
import           Language.Haskell.TH.Syntax  (Lift)
import           Numeric                     (showHex)

data QuadBitboard = QBB { black, pbq, nbk, rqk :: {-# UNPACK #-} !Word64 }
                    deriving (Eq, Generic, Lift, Ord)

instance NFData QuadBitboard

occupied, white, pawns, knights, bishops, rooks, queens, kings :: QuadBitboard -> Word64
occupied = liftA2 (.|.) pbq $ liftA2 (.|.) nbk rqk
white    = liftA2  xor  occupied black
pawns    = liftA2 (.&.) pbq $ liftA2 (.&.) (complement . nbk) (complement . rqk)
knights  = liftA2 (.&.) (complement . pbq) $ liftA2 (.&.) nbk (complement . rqk)
bishops  = liftA2 (.&.) pbq nbk
rooks    = liftA2 (.&.) (complement . pbq) $ liftA2 (.&.) (complement . nbk) rqk
queens   = liftA2 (.&.) pbq rqk
kings    = liftA2 (.&.) nbk rqk

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

orthogonals, diagonals :: QuadBitboard -> Word64
diagonals = liftA2 xor pbq pawns
orthogonals = liftA2 xor rqk kings


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
{-# INLINE diagonals #-}
{-# INLINE orthogonals #-}



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

instance IsList QuadBitboard where
  type Item QuadBitboard = (Square, Word4)
  fromList = mconcat . map (uncurry singleton)
  toList qbb = go maxBound [] where
    go sq xs
      | sq /= minBound = go (pred sq) xs'
      | otherwise      = xs'
     where nb = qbb ! sq
           xs' | nb /= NoPiece = (sq, nb) : xs
               | otherwise     = xs

empty, standard :: QuadBitboard
empty = QBB 0 0 0 0
standard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

-- | bitwise XOR
instance Semigroup QuadBitboard where
  {-# INLINE (<>) #-}
  QBB b0 b1 b2 b3 <> QBB b0' b1' b2' b3' =
    QBB (b0 `xor` b0') (b1 `xor` b1') (b2 `xor` b2') (b3 `xor` b3')

instance Monoid QuadBitboard where
  mempty = empty

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
    poke (castPtr p) black
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

newtype Word4 = W4 Word8
              deriving (Bits, Eq, Integral, Ix, Num, Ord, Read, Real)

instance Show Word4 where
  show NoPiece     = "NoPiece"
  show WhiteKing   = "WhiteKing"
  show WhitePawn   = "WhitePawn"
  show WhiteKnight = "WhiteKnight"
  show WhiteBishop = "WhiteBishop"
  show WhiteRook   = "WhiteRook"
  show WhiteQueen  = "WhiteQueen"
  show BlackKing   = "BlackKing"
  show BlackPawn   = "BlackPawn"
  show BlackKnight = "BlackKnight"
  show BlackBishop = "BlackBishop"
  show BlackRook   = "BlackRook"
  show BlackQueen  = "BlackQueen"
  show (W4 n)      = "W4 " <> show n

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
  countLeadingZeros (W4 x) = countLeadingZeros x - 4
  countTrailingZeros (W4 x) = countTrailingZeros x

-- | law: singleton i x ! i = x where inRange (0,63) i && inRange (0,15) x
{-# INLINE singleton #-}
singleton :: Square -> Word4 -> QuadBitboard
singleton (Sq sq) !nb = QBB (f 0) (f 1) (f 2) (f 3) where
  !b = 1 `unsafeShiftL` sq
  f !n = fromIntegral ((nb `unsafeShiftR` n) .&. 1) * b

(!) :: QuadBitboard -> Square -> Word4
(!) QBB{..} (Sq sq) = fromIntegral $ f black 0 .|. f pbq 1 .|. f nbk 2 .|. f rqk 3 where
  f !bb !n = ((bb `unsafeShiftR` sq) .&. 1) `unsafeShiftL` n

{-# INLINE (!) #-}

setNibble :: Bits nibble => QuadBitboard -> Int -> nibble -> QuadBitboard
setNibble QBB{..} sq nb = QBB (f 0 black) (f 1 pbq) (f 2 nbk) (f 3 rqk) where
  f n | nb `testBit` n = (`setBit` sq)
      | otherwise      = (`clearBit` sq)

instance Binary QuadBitboard

instance IsString QuadBitboard where
  fromString = go (view rankFile A8) mempty where
    go _ !qbb "" = qbb
    go (r, _) qbb ('/':xs) = go (pred r, FileA) qbb xs
    go rf@(r, f) !qbb (x:xs)
      | inRange ('1','8') x = go (r, mkFile $ unFile f + ord x - ord '0') qbb xs
      | otherwise = go (r, succ f) (qbb <> singleton sq nb) xs where
        sq = view (from rankFile) rf
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

instance Hashable QuadBitboard where
  hashWithSalt s QBB{black, pbq, nbk, rqk} =
    s `hashWithSalt` black `hashWithSalt` pbq `hashWithSalt` nbk `hashWithSalt` rqk

instance Show QuadBitboard where
  show QBB{..} =
     "QBB {black = 0x" <> showHex black
    (", pbq = 0x" <> showHex pbq
    (", nbk = 0x" <> showHex nbk
    (", rqk = 0x" <> showHex rqk "}")))

toString :: QuadBitboard -> String
toString qbb = intercalate "/" $ rnk <$> [Rank8, Rank7 .. Rank1] where
  rnk r = concatMap countEmpty . groupBy spaces $ charAt r <$> [FileA .. FileH]
  countEmpty xs | head xs == spc = show $ length xs
                | otherwise      = xs
  spaces x y = x == spc && x == y
  charAt r f = maybe spc (if odd nb then toLower else id) $
    lookup (nb `div` 2) $ zip [1..] "PNBRQK"
   where nb = qbb ! ((r, f) ^. from rankFile)
  spc = ' '

-- | Move a nibble.  Note that this function, while convenient, isn't very
-- fast as it needs to lookup the source nibble value.
move :: QuadBitboard -> Square -> Square -> QuadBitboard
move qbb fromSq toSq = qbb <> move' fromSq (qbb ! fromSq) toSq (qbb ! toSq)

move' :: Square -> Word4 -> Square -> Word4 -> QuadBitboard
move' fromSq fromCode toSq toCode =
  singleton fromSq fromCode <> singleton toSq (fromCode `xor` toCode)

{-# INLINE move' #-}

whiteKingsideCastle, whiteQueensideCastle, blackKingsideCastle, blackQueensideCastle
  :: QuadBitboard
whiteKingsideCastle  = move' E1 WhiteKing G1 NoPiece <> move' H1 WhiteRook F1 NoPiece
whiteQueensideCastle = move' E1 WhiteKing C1 NoPiece <> move' A1 WhiteRook D1 NoPiece
blackKingsideCastle  = move' E8 BlackKing G8 NoPiece <> move' H8 BlackRook F8 NoPiece
blackQueensideCastle = move' E8 BlackKing C8 NoPiece <> move' A8 BlackRook D8 NoPiece

enPassant :: Square -> Square -> QuadBitboard
enPassant fromSq toSq
  | unSquare fromSq < unSquare toSq
  = move' fromSq WhitePawn toSq NoPiece <> singleton (mkSq $ unSquare toSq - 8) BlackPawn
  | otherwise
  = move' fromSq BlackPawn toSq NoPiece <> singleton (mkSq $ unSquare toSq + 8) WhitePawn

whitePromotion, blackPromotion :: QuadBitboard -> Square -> Square -> Word4 -> QuadBitboard
whitePromotion qbb fromSq toSq promoCode =
  qbb <> whitePromotion' fromSq toSq (qbb ! toSq) promoCode
blackPromotion qbb fromSq toSq promoCode =
  qbb <> blackPromotion' fromSq toSq (qbb ! toSq) promoCode

whitePromotion', blackPromotion' :: Square -> Square -> Word4 -> Word4 -> QuadBitboard
whitePromotion' fromSq toSq toCode promoCode =
  singleton fromSq WhitePawn <> singleton toSq (toCode `xor` promoCode)
blackPromotion' fromSq toSq toCode promoCode =
  singleton fromSq BlackPawn <> singleton toSq (toCode `xor` promoCode)
