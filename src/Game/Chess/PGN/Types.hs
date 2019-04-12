module Game.Chess.PGN.Types where

import Data.ByteString (ByteString)
import Data.Tree
import Game.Chess

type PGN = [([(ByteString, String)], (Outcome, Forest PlyData))]

data Outcome = Win Color
             | Draw
             | Undecided
             deriving (Eq, Show)

data PlyData = PlyData {
  prefixNAG :: ![Int]
, ply :: !Move
, suffixNAG :: ![Int]
} deriving Show
