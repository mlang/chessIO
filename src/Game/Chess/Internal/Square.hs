module Game.Chess.Internal.Square where

import Data.Bits (Bits(testBit))
import Data.Char (chr, ord)
import Data.Coerce (coerce)
import Data.Ix (Ix(..))
import Data.String (IsString(fromString))
import Data.Word
import GHC.Stack (HasCallStack)

newtype Rank = Rank Int deriving (Eq, Ord)

unRank :: Rank -> Int
unRank = coerce

mkRank :: HasCallStack => Int -> Rank
mkRank n | n >= 0 && n <= 7 = Rank n
         | otherwise        = error $ "mkRank " ++ show n

instance Show Rank where
  showsPrec _ (Rank 0) = showString "Rank1"
  showsPrec _ (Rank 1) = showString "Rank2"
  showsPrec _ (Rank 2) = showString "Rank3"
  showsPrec _ (Rank 3) = showString "Rank4"
  showsPrec _ (Rank 4) = showString "Rank5"
  showsPrec _ (Rank 5) = showString "Rank6"
  showsPrec _ (Rank 6) = showString "Rank7"
  showsPrec _ (Rank 7) = showString "Rank8"
  showsPrec d (Rank n) = showParen (d > 10) $
                         showString "Rank " . showsPrec 11 n

instance Enum Rank where
  toEnum n | n >= 0 && n <= 7 = Rank n
           | otherwise        = error $ "Rank out-of-bound " ++ show n
  fromEnum = coerce

instance Bounded Rank where
  minBound = Rank1
  maxBound = Rank8

pattern Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8 :: Rank
pattern Rank1 = Rank 0
pattern Rank2 = Rank 1
pattern Rank3 = Rank 2
pattern Rank4 = Rank 3
pattern Rank5 = Rank 4
pattern Rank6 = Rank 5
pattern Rank7 = Rank 6
pattern Rank8 = Rank 7

{-# COMPLETE Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8 :: Rank #-}

newtype File = File Int deriving (Eq, Ord)

unFile :: File -> Int
unFile = coerce

mkFile :: HasCallStack => Int -> File
mkFile n | n >= 0 && n <= 7 = File n
         | otherwise        = error $ "mkFile " ++ show n

instance Show File where
  showsPrec _ (File 0) = showString "FileA"
  showsPrec _ (File 1) = showString "FileB"
  showsPrec _ (File 2) = showString "FileC"
  showsPrec _ (File 3) = showString "FileD"
  showsPrec _ (File 4) = showString "FileE"
  showsPrec _ (File 5) = showString "FileF"
  showsPrec _ (File 6) = showString "FileG"
  showsPrec _ (File 7) = showString "FileH"
  showsPrec d (File n) = showParen (d > 10) $
                         showString "File " . showsPrec 11 n

instance Enum File where
  toEnum n | n >= 0 && n <= 7 = File n
           | otherwise        = error $ "File out-of-bound " ++ show n
  fromEnum = coerce

instance Bounded File where
  minBound = FileA
  maxBound = FileH

pattern FileA, FileB, FileC, FileD, FileE, FileF, FileG, FileH :: File
pattern FileA = File 0
pattern FileB = File 1
pattern FileC = File 2
pattern FileD = File 3
pattern FileE = File 4
pattern FileF = File 5
pattern FileG = File 6
pattern FileH = File 7

{-# COMPLETE FileA, FileB, FileC, FileD, FileE, FileF, FileG, FileH :: File #-}

newtype Square = Sq Int deriving (Eq, Ord)

instance Ix Square where
  range (Sq i, Sq j) = [Sq k | k <- [i..j]]
  index (Sq i, Sq j) (Sq k) = index (i, j) k
  inRange (Sq i, Sq j) (Sq k) = inRange (i, j) k
  rangeSize (Sq i, Sq j) = j - i

unSq :: Square -> Int
unSq = coerce

-- TODO: this check is expensive, maybe only worth in "debug" builds.
mkSq :: HasCallStack => Int -> Square
mkSq n | n >= 0 && n <= 63 = Sq n
       | otherwise         = error $ "mkSq " ++ show n

instance Show Square where
  showsPrec d (Sq i) | i >= 0 && i <= 63 = showString [f', r']
                     | otherwise         = showParen (d > 10) $
                                           showString "Sq " . showsPrec 11 i
      where
        (r, f) = i `divMod` 8
        r' = chr (r + ord '1')
        f' = chr (f + ord 'A')

instance Enum Square where
  toEnum n | n >= 0 && n <= 63 = Sq n
           | otherwise         = error $ "Sq out-of-bound " ++ show n
  fromEnum = coerce

instance Bounded Square where
  minBound = A1
  maxBound = H8

pattern A1, A2, A3, A4, A5, A6, A7, A8 :: Square
pattern B1, B2, B3, B4, B5, B6, B7, B8 :: Square
pattern C1, C2, C3, C4, C5, C6, C7, C8 :: Square
pattern D1, D2, D3, D4, D5, D6, D7, D8 :: Square
pattern E1, E2, E3, E4, E5, E6, E7, E8 :: Square
pattern F1, F2, F3, F4, F5, F6, F7, F8 :: Square
pattern G1, G2, G3, G4, G5, G6, G7, G8 :: Square
pattern H1, H2, H3, H4, H5, H6, H7, H8 :: Square

pattern A1 = Sq 0
pattern B1 = Sq 1
pattern C1 = Sq 2
pattern D1 = Sq 3
pattern E1 = Sq 4
pattern F1 = Sq 5
pattern G1 = Sq 6
pattern H1 = Sq 7

pattern A2 = Sq 8
pattern B2 = Sq 9
pattern C2 = Sq 10
pattern D2 = Sq 11
pattern E2 = Sq 12
pattern F2 = Sq 13
pattern G2 = Sq 14
pattern H2 = Sq 15

pattern A3 = Sq 16
pattern B3 = Sq 17
pattern C3 = Sq 18
pattern D3 = Sq 19
pattern E3 = Sq 20
pattern F3 = Sq 21
pattern G3 = Sq 22
pattern H3 = Sq 23

pattern A4 = Sq 24
pattern B4 = Sq 25
pattern C4 = Sq 26
pattern D4 = Sq 27
pattern E4 = Sq 28
pattern F4 = Sq 29
pattern G4 = Sq 30
pattern H4 = Sq 31

pattern A5 = Sq 32
pattern B5 = Sq 33
pattern C5 = Sq 34
pattern D5 = Sq 35
pattern E5 = Sq 36
pattern F5 = Sq 37
pattern G5 = Sq 38
pattern H5 = Sq 39

pattern A6 = Sq 40
pattern B6 = Sq 41
pattern C6 = Sq 42
pattern D6 = Sq 43
pattern E6 = Sq 44
pattern F6 = Sq 45
pattern G6 = Sq 46
pattern H6 = Sq 47

pattern A7 = Sq 48
pattern B7 = Sq 49
pattern C7 = Sq 50
pattern D7 = Sq 51
pattern E7 = Sq 52
pattern F7 = Sq 53
pattern G7 = Sq 54
pattern H7 = Sq 55

pattern A8 = Sq 56
pattern B8 = Sq 57
pattern C8 = Sq 58
pattern D8 = Sq 59
pattern E8 = Sq 60
pattern F8 = Sq 61
pattern G8 = Sq 62
pattern H8 = Sq 63

rank :: Square -> Rank
rank = Rank . (`div` 8) . coerce

file :: Square -> File
file = File . (`mod` 8) . coerce

rankFile :: Square -> (Rank, File)
rankFile sq = case unSq sq `divMod` 8 of (r, f) -> (Rank r, File f)

mkSqRF :: (Rank, File) -> Square
mkSqRF (Rank r, File f) = Sq $ r*8 + f

fileChar, rankChar :: Square -> Char
fileChar = chr . (ord 'a' +) . unFile . file
rankChar = chr . (ord '1' +) . unRank . rank

toCoord :: IsString s => Square -> s
toCoord sq = fromString $ ($ sq) <$> [fileChar, rankChar]

isDark :: Square -> Bool
isDark sq = (0xaa55aa55aa55aa55 :: Word64) `testBit` unSq sq

isLight :: Square -> Bool
isLight = not . isDark
