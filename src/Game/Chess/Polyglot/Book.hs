module Game.Chess.Polyglot.Book (
  PolyglotBook
, readPolyglotFile
, bookPlies
, bookForest
) where

import Control.Applicative
import Data.Bits
import Data.Binary
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unboxed
import Data.Ix
import Data.Tree
import Game.Chess
import Game.Chess.Polyglot.Hash

data BookEntry = BookEntry {
  key :: !Word64
, ply :: !Ply
, weight :: !Word16
, learn :: !Word32
} deriving (Eq, Show)

instance Binary BookEntry where
  get = BookEntry <$> get <*> get <*> get <*> get
  put BookEntry{key, ply, weight, learn} =
    put key *> put ply *> put weight *> put learn

newtype PolyglotBook = Book { unBook :: Vector BookEntry }

instance Binary PolyglotBook where
  get = Book . Vector.fromList <$> many get
  put = traverse_ put . unBook

readPolyglotFile :: FilePath -> IO PolyglotBook
readPolyglotFile = decodeFile 

bookForest :: PolyglotBook -> Position -> Forest Ply
bookForest b p = tree <$> bookPlies b p where
  tree pl = Node pl . bookForest b $ unsafeDoPly p pl

bookPlies :: PolyglotBook -> Position -> [Ply]
bookPlies (Book v) pos
  | halfMoveClock pos > 150 = []
  | otherwise = fmap ply . Vector.toList . Vector.takeWhile ((hash ==) . key) $
    Vector.unsafeDrop (lowerBound v hash) v
 where
  hash = hashPosition pos
  lowerBound v = bsearch (key . Vector.unsafeIndex v) (0, Vector.length v - 1)
  bsearch :: (Integral a, Ord b) => (a -> b) -> (a, a) -> b -> a
  bsearch f (lo, hi) x
    | lo >= hi   = lo
    | x <= f mid = bsearch f (lo, mid) x
    | otherwise  = bsearch f (mid + 1, hi) x
   where mid = (lo + hi) `div` 2
