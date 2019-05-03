{-# LANGUAGE TemplateHaskell #-}
module Game.Chess.Polyglot.Book (
  PolyglotBook
, defaultBook, twic
, readPolyglotFile
, bookPlies
, bookForest
) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector as Vector
import Data.Tree
import Game.Chess
import Game.Chess.Polyglot.Hash

defaultBook, twic :: PolyglotBook
defaultBook = twic
twic = (decode . fromStrict) $(embedFile "book/twic-9g.bin")

pv :: PolyglotBook -> [Ply]
pv b = head . concatMap paths $ bookForest b startpos

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

newtype instance Unboxed.MVector s BookEntry = MV_BookEntry (Unboxed.MVector s (Word64,Ply,Word16,Word32))
newtype instance Unboxed.Vector    BookEntry = V_BookEntry  (Unboxed.Vector    (Word64,Ply,Word16,Word32))
instance Unboxed.Unbox BookEntry

instance M.MVector Unboxed.MVector BookEntry where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_BookEntry v) = M.basicLength v
  basicUnsafeSlice i n (MV_BookEntry v) = MV_BookEntry $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_BookEntry v1) (MV_BookEntry v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_BookEntry <$> M.basicUnsafeNew n
  basicInitialize (MV_BookEntry v) = M.basicInitialize v
  basicUnsafeReplicate n (BookEntry a b c d) = MV_BookEntry <$> M.basicUnsafeReplicate n (a,b,c,d)
  basicUnsafeRead (MV_BookEntry v) i = (\(a, b, c, d) -> BookEntry a b c d) <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_BookEntry v) i (BookEntry a b c d) = M.basicUnsafeWrite v i (a,b,c,d)
  basicClear (MV_BookEntry v) = M.basicClear v
  basicSet (MV_BookEntry v) (BookEntry a b c d) = M.basicSet v (a,b,c,d)
  basicUnsafeCopy (MV_BookEntry v1) (MV_BookEntry v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_BookEntry v1) (MV_BookEntry v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_BookEntry v) n = MV_BookEntry <$> M.basicUnsafeGrow v n

instance G.Vector Unboxed.Vector BookEntry where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_BookEntry v) = V_BookEntry <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_BookEntry v) = MV_BookEntry <$> G.basicUnsafeThaw v
  basicLength (V_BookEntry v) = G.basicLength v
  basicUnsafeSlice i n (V_BookEntry v) = V_BookEntry $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_BookEntry v) i =
    (\(a, b, c, d) -> BookEntry a b c d) <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_BookEntry mv) (V_BookEntry v) = G.basicUnsafeCopy mv v
  elemseq _ (BookEntry a b c d) z = G.elemseq (undefined :: Vector a) a
                                  $ G.elemseq (undefined :: Vector a) b
                                  $ G.elemseq (undefined :: Vector a) c
                                  $ G.elemseq (undefined :: Vector a) d z

newtype PolyglotBook = Book { unBook :: Vector BookEntry }

instance Binary PolyglotBook where
  get = Book . Vector.fromList <$> many get
  put = traverse_ put . unBook

readPolyglotFile :: FilePath -> IO PolyglotBook
readPolyglotFile = decodeFile 

bookForest :: PolyglotBook -> Position -> Forest Ply
bookForest b p = tree <$> bookPlies b p where
  tree pl = Node pl . bookForest b $ unsafeDoPly p pl

paths :: Tree a -> [[a]]
paths = foldTree f where
  f a [] = [[a]]
  f a xs = (a :) <$> concat xs

bookPlies :: PolyglotBook -> Position -> [Ply]
bookPlies (Book v) pos
  | halfMoveClock pos > 150 = []
  | otherwise = fmap ply . Vector.toList . Vector.takeWhile ((hash ==) . key) $
    Vector.unsafeDrop (lowerBound hash) v
 where
  hash = hashPosition pos
  lowerBound = bsearch (key . Vector.unsafeIndex v) (0, Vector.length v - 1)
  bsearch :: (Integral a, Ord b) => (a -> b) -> (a, a) -> b -> a
  bsearch f (lo, hi) x
    | lo >= hi   = lo
    | x <= f mid = bsearch f (lo, mid) x
    | otherwise  = bsearch f (mid + 1, hi) x
   where mid = (lo + hi) `div` 2
