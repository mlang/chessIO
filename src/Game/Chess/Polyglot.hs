{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Game.Chess.Polyglot (
  -- * Data type
  PolyglotBook, BookEntry(..), beKey, bePly, beWeight, beLearn
  -- * Built-in books
, defaultBook, twic
  -- * Load and save
, fromByteString, toByteString
, readPolyglotFile, writePolyglotFile
, makeBook, toPGN
  -- * Lookup
, bookPly
, bookPlies
, bookForest
, variations
, findPosition
) where

import           Control.Arrow            (Arrow ((&&&)))
import           Control.Lens             (makeLenses, (%~))
import           Control.Monad.Random     (Rand)
import qualified Control.Monad.Random     as Rand
import           Data.Bits                (Bits (shiftL, shiftR, (.|.)))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import           Data.FileEmbed           (embedFile)
import           Data.Foldable            (fold)
import           Data.Hashable
import           Data.List                (sort)
import           Data.Ord                 (Down (Down))
import           Data.String              (IsString (fromString))
import           Data.Tree                (Forest, Tree (Node), foldTree)
import Data.Vector.Instances
import qualified Data.Vector.Storable     as VS
import           Data.Word                (Word16, Word32, Word64, Word8)
import           Foreign.ForeignPtr       (castForeignPtr, plusForeignPtr)
import           Foreign.Storable         (Storable (alignment, peek, poke, pokeElemOff, sizeOf))
import           GHC.Generics (Generic)
import           GHC.Ptr                  (Ptr, castPtr, plusPtr)
import           Game.Chess.Internal      (Ply (..), Position (halfMoveClock),
                                           doPly, fromPolyglot, startpos, toFEN,
                                           toPolyglot, unsafeDoPly)
import           Game.Chess.PGN           (Outcome (Undecided), PGN (..),
                                           gameFromForest, weightedForest)
import           Game.Chess.Polyglot.Hash (hashPosition)
import           System.Random            (RandomGen)

data BookEntry = BookEntry {
  _beKey    :: {-# UNPACK #-} !Word64
, _bePly    :: {-# UNPACK #-} !Ply
, _beWeight :: {-# UNPACK #-} !Word16
, _beLearn  :: {-# UNPACK #-} !Word32
} deriving (Eq, Generic, Show)

instance Hashable BookEntry

makeLenses ''BookEntry

instance Ord BookEntry where
  compare (BookEntry k1 _ w1 _) (BookEntry k2 _ w2 _) =
    compare k1 k2 <> compare (Down w1) (Down w2)

instance Storable BookEntry where
  sizeOf _ = 16
  alignment _ = alignment (undefined :: Word64)
  peek ptr = BookEntry <$> peekBE (castPtr ptr)
                       <*> (Ply <$> peekBE (castPtr ptr `plusPtr` 8))
                       <*> peekBE (castPtr ptr `plusPtr` 10)
                       <*> peekBE (castPtr ptr `plusPtr` 12)
  poke ptr (BookEntry key (Ply ply) weight learn) = do
    pokeBE (castPtr ptr) key
    pokeBE (castPtr ptr `plusPtr` 8) ply
    pokeBE (castPtr ptr `plusPtr` 10) weight
    pokeBE (castPtr ptr `plusPtr` 12) learn

peekBE :: forall a. (Bits a, Num a, Storable a) => Ptr Word8 -> IO a
peekBE ptr = go ptr 0 (sizeOf (undefined :: a)) where
  go _ !x 0 = pure x
  go !p !x !n = peek p >>= \w8 ->
    go (p `plusPtr` 1) (x `shiftL` 8 .|. fromIntegral w8) (n - 1)

pokeBE :: forall a. (Bits a, Integral a, Num a, Storable a) => Ptr Word8 -> a -> IO ()
pokeBE p x = go x (sizeOf x) where
  go _ 0   = pure ()
  go !v !n = pokeElemOff p (n-1) (fromIntegral v) *> go (v `shiftR` 8) (n-1)

defaultBook, twic :: PolyglotBook
defaultBook = twic
twic = fromByteString $(embedFile "book/twic-9g.bin")

-- | A Polyglot opening book.
newtype PolyglotBook = Book (VS.Vector BookEntry) deriving (Eq, Hashable)

instance Semigroup PolyglotBook where
  Book a <> Book b = fromList . VS.toList $ a <> b

instance Monoid PolyglotBook where
  mempty = Book mempty

-- | Create a PolyglotBook from a ByteString.
fromByteString :: ByteString -> PolyglotBook
fromByteString bs = Book v where
  v = VS.unsafeFromForeignPtr0 (plusForeignPtr fptr off) (len `div` elemSize)
  (fptr, off, len) = BS.toForeignPtr bs
  elemSize = sizeOf (undefined `asTypeOf` VS.head v)

toByteString :: PolyglotBook -> ByteString
toByteString (Book v) = BS.fromForeignPtr (castForeignPtr fptr) off (len * elemSize)
 where
  (fptr, off, len) = VS.unsafeToForeignPtr v
  elemSize = sizeOf (undefined `asTypeOf` VS.head v)

readPolyglotFile :: FilePath -> IO PolyglotBook
readPolyglotFile = fmap fromByteString . BS.readFile

writePolyglotFile :: FilePath -> PolyglotBook -> IO ()
writePolyglotFile fp = BS.writeFile fp . toByteString

fromList :: [BookEntry] -> PolyglotBook
fromList = Book . VS.fromList . sort

--toList :: PolyglotBook -> [BookEntry]
--toList (Book v) = VS.toList v

toPGN :: PolyglotBook -> Position -> PGN
toPGN b p = PGN [gameFromForest meta (bookForest b p) Undecided] where
  meta | p == startpos = []
       | otherwise     = [("FEN", fromString $ toFEN p)]

makeBook :: PGN -> PolyglotBook
makeBook = fromList . concatMap (foldTree f . annot startpos) . weightedForest
 where
  annot pos (Node a ts) =
    Node (pos, a) $ annot (unsafeDoPly pos (snd a)) <$> ts
  f (pos, (w, pl)) xs
    | w > 0
    = BookEntry (hashPosition pos) (toPolyglot pos pl) (floor w) 0 : concat xs
    | otherwise
    = concat xs

bookForest :: PolyglotBook -> Position -> Forest Ply
bookForest b = (fmap . fmap) (snd . head) . forest [] where
  forest pls p = tree pls p <$> filter (not . seen pls) (plies p)
  tree pls p (pl, p') = Node pls' $ forest pls' p' where pls' = (p, pl) : pls
  plies p = f <$> bookPlies b p where f pl = (pl, doPly p pl)
  seen pls (_, p') = p' `elem` map fst pls

-- | Pick a random ply from the book.
bookPly :: RandomGen g => PolyglotBook -> Position -> Maybe (Rand g Ply)
bookPly b pos = case findPosition b pos of
  [] -> Nothing
  l  -> Just . Rand.fromList $ map (_bePly &&& fromIntegral . _beWeight) l

-- | Probe the book for all plies known for the given position.
bookPlies :: PolyglotBook -> Position -> [Ply]
bookPlies b pos
  | halfMoveClock pos > 150 = []
  | otherwise = _bePly <$> findPosition b pos

-- | Predicted Variations.  Return the most popular game.
variations :: PolyglotBook -> Position -> [[Ply]]
variations b = concatMap (foldTree f) . bookForest b where
  f a [] = [[a]]
  f a xs = (a :) <$> fold xs

findPosition :: PolyglotBook -> Position -> [BookEntry]
findPosition (Book v) pos =
  fmap (bePly %~ fromPolyglot pos) .
  VS.toList .
  VS.takeWhile ((hash ==) . _beKey) .
  VS.unsafeDrop (lowerBound hash) $ v
 where
  hash = hashPosition pos
  lowerBound = bsearch (_beKey . VS.unsafeIndex v) (0, VS.length v - 1)

bsearch :: (Integral a, Ord b) => (a -> b) -> (a, a) -> b -> a
bsearch f (lo, hi) x
  | lo >= hi   = lo
  | x <= f mid = bsearch f (lo, mid) x
  | otherwise  = bsearch f (mid + 1, hi) x
 where mid = lo + ((hi - lo) `div` 2)
