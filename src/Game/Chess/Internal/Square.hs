module Game.Chess.Internal.Square (
  IsSquare(..), Sq(..), isDark, isLight, fileChar, rankChar, toCoord
) where

import Data.Bits (Bits(testBit))
import Data.Char (chr, ord)
import Data.Ix (Ix)
import Data.String (IsString(fromString))
import Data.Word

class IsSquare sq where
  toIndex :: sq -> Int
  toRF :: sq -> (Int, Int)
  toRF sq = toIndex sq `divMod` 8
  rankOf :: sq -> Int
  rankOf sq = toIndex sq `div` 8
  fileOf :: sq -> Int
  fileOf sq = toIndex sq `mod` 8

instance IsSquare Int where
  toIndex = id

instance IsSquare (Int, Int) where
  toIndex (r, f) = r*8 + f
  toRF = id
  rankOf = fst
  fileOf = snd

data Sq = A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1
        | A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2
        | A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3
        | A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4
        | A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5
        | A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6
        | A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7
        | A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8
        deriving (Bounded, Enum, Eq, Ix, Ord, Show, Read)

instance IsSquare Sq where
  toIndex = fromEnum

fileChar, rankChar :: IsSquare sq => sq -> Char
fileChar = chr . (ord 'a' +) . fileOf
rankChar = chr . (ord '1' +) . rankOf

toCoord :: (IsSquare sq, IsString s) => sq -> s
toCoord sq = fromString $ ($ sq) <$> [fileChar, rankChar]

isDark :: IsSquare sq => sq -> Bool
isDark sq = (0xaa55aa55aa55aa55 :: Word64) `testBit` toIndex sq

isLight :: IsSquare sq => sq -> Bool
isLight = not . isDark
