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
import           Control.Monad.Primitive (primToST)
import           Control.Monad.ST (ST, runST)
import           Control.Monad.Random     (Rand)
import qualified Control.Monad.Random     as Rand
import           Data.Bits                (Bits (shiftL, shiftR, (.|.)))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import           Data.FileEmbed           (embedFile)
import           Data.Foldable            (fold)
import           Data.Hashable            (Hashable)
import           Data.List                (sort)
import           Data.Ord                 (Down (Down))
import           Data.String              (IsString (fromString))
import           Data.Tree                (Tree (Node), foldTree)
import qualified Data.Vector.Algorithms.Intro as V
import           Data.Vector.Instances    ()
import qualified Data.Vector.Storable     as VS
import           Data.Word                (Word16, Word32, Word64, Word8)
import           Foreign.ForeignPtr       (castForeignPtr, plusForeignPtr)
import           Foreign.Storable         (Storable (alignment, peek, poke, pokeElemOff, sizeOf))
import           GHC.Generics             (Generic)
import           GHC.Ptr                  (Ptr, castPtr, plusPtr)
import           Game.Chess.Internal      (Ply (..), unpack, move, Color(..), Position (color, halfMoveClock), canCastleQueenside, canCastleKingside, wKscm, bKscm, wQscm, bQscm, 
                                           doPly, startpos, toFEN,
                                           unsafeDoPly)
import           Game.Chess.Internal.Square
import           Game.Chess.PGN           (Outcome (Undecided), PGN (..),
                                           gameFromForest, weightedForest)
import           Game.Chess.Polyglot.Hash (hashPosition)
import           System.Random            (RandomGen)

data BookEntry a = BE {
  _beKey    :: {-# UNPACK #-} !Word64
, _bePly    :: {-# UNPACK #-} !a
, _beWeight :: {-# UNPACK #-} !Word16
, _beLearn  :: {-# UNPACK #-} !Word32
} deriving (Eq, Functor, Generic, Show)

instance Hashable a => Hashable (BookEntry a)

makeLenses ''BookEntry

instance Ord a => Ord (BookEntry a) where
  compare (BE k1 p1 w1 _) (BE k2 p2 w2 _) =
    k1 `compare` k2 <> Down w1 `compare` Down w2 <> p1 `compare` p2

instance Storable (BookEntry Word16) where
  sizeOf _ = 16
  alignment _ = alignment (undefined :: Word64)
  peek ptr = BE <$> peekBE (castPtr ptr)
                <*> peekBE (castPtr ptr `plusPtr` 8)
                <*> peekBE (castPtr ptr `plusPtr` 10)
                <*> peekBE (castPtr ptr `plusPtr` 12)
  poke ptr BE { .. } = do
    pokeBE (castPtr ptr) _beKey
    pokeBE (castPtr ptr `plusPtr` 8) _bePly
    pokeBE (castPtr ptr `plusPtr` 10) _beWeight
    pokeBE (castPtr ptr `plusPtr` 12) _beLearn

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
newtype PolyglotBook = Book (VS.Vector (BookEntry Word16)) deriving (Eq, Hashable)

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

fromList :: [BookEntry Word16] -> PolyglotBook
fromList = Book . VS.fromList . sort

--toList :: PolyglotBook -> [BookEntry Word16]
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
    = BE (hashPosition pos) (fromPly pos pl) (floor w) 0 : concat xs
    | otherwise
    = concat xs

bookForest :: PolyglotBook -> Position -> [Tree Ply]
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

findPosition :: PolyglotBook -> Position -> [BookEntry Ply]
findPosition (Book v) pos =
  (fmap . fmap) (toPly pos) .
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

toPly :: Position -> Word16 -> Ply
toPly pos pl@(unpack . Ply -> (src, dst, _)) = case color pos of
  White | src == E1
        , canCastleKingside pos
        , dst == H1
        -> wKscm
        | src == E1
        , canCastleQueenside pos
        , dst == A1
        -> wQscm
  Black | src == E8
        , canCastleKingside pos
        , dst == H8
        -> bKscm
        | src == E8
        , canCastleQueenside pos
        , dst == A8
        -> bQscm
  _ -> Ply pl

fromPly :: Position -> Ply -> Word16
fromPly pos pl@(unpack -> (src, dst, _)) = unPly $ case color pos of
  White | src == E1
        , canCastleKingside pos
        , dst == G1
        -> src `move` H1
        | src == E1
        , canCastleQueenside pos
        , dst == C1
        -> src `move` A1
  Black | src == E8
        , canCastleKingside pos
        , dst == G8
        -> src `move` H8
        | src == E8
        , canCastleQueenside pos
        , dst == C8
        -> src `move` A8
  _ -> pl

