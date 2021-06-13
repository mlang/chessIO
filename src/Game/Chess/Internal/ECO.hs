{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.Chess.Internal.ECO where

import           Control.DeepSeq
import           Control.Exception          (Exception (displayException),
                                             throwIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Binary
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as Lazy
import           Data.Char
import           Data.Data
import           Data.Either                (fromRight)
import           Data.FileEmbed             (embedFile)
import           Data.Foldable              (fold)
import           Data.Functor
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Hashable              (Hashable)
import           Data.Maybe
import           Data.MonoTraversable       (MonoFoldable (ofoldl'))
import           Data.Ord
import           Data.Ratio
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tree                  (foldTree)
import           Data.Typeable              (Typeable)
import           Data.Vector.Binary
import           Data.Vector.Instances
import qualified Data.Vector.Unboxed        as Unboxed
import           Data.Void
import           Data.Word                  (Word8)
import           GHC.Generics               (Generic)
import           Game.Chess
import           Game.Chess.PGN
import           Game.Chess.SAN
import           Instances.TH.Lift
import           Language.Haskell.TH.Syntax (Lift, Q, TExp, liftTyped)
import           Prelude                    hiding (lookup)
import qualified Prelude                    as Prelude
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

-- | A Chess Opening
data Opening = CO {
  coCode      :: !Text
, coName      :: !Text
, coVariation :: !(Maybe Text)
, coPlies     :: !(Unboxed.Vector Ply)
} deriving (Eq, Generic, Lift, Show)

instance Binary Opening
instance Hashable Opening
instance NFData Opening

type FileReader = forall m. MonadIO m => FilePath -> m (Either String [Opening])

-- | Parse an ECO database in .pgn format
eco_pgn :: FileReader
eco_pgn fp = fmap fromPGN' <$> readPGNFile fp

-- | Parse an ECO database in .eco format
scid_eco :: FileReader
scid_eco fp = first errorBundlePretty . parse scid' fp <$> liftIO (BS.readFile fp)

-- | Encyclopedia of Chess Openings
newtype ECO = ECO { toHashMap :: HashMap Position Opening }
              deriving (Eq, NFData, Hashable, Semigroup, Monoid)

instance Binary ECO where
  put = put . toList
  get = fromList <$> get

embedECO :: FileReader -> FilePath -> Q (TExp ECO)
embedECO load fp = (fmap.fmap) liftTyped (load fp) >>= \case
  Right xs -> [|| fromList $$(xs) ||]
  Left err -> fail err

toList :: ECO -> [Opening]
toList = map snd . HashMap.toList . toHashMap

fromList :: [Opening] -> ECO
fromList = ECO . HashMap.fromList . fmap (\co -> (pos co, co)) where
  pos = ofoldl' unsafeDoPly startpos . coPlies

-- | Convert a PGN database to ECO assuming the ECO, Opening and Variation tags are
-- being used to identify chess openings.
fromPGN :: PGN -> ECO
fromPGN = fromList . fromPGN'

fromPGN' :: PGN -> [Opening]
fromPGN' (PGN games) = map mkCO games where
  mkCO (tags, (_, forest)) = CO { .. } where
    coCode = fromMaybe "" $ Prelude.lookup "ECO" tags
    coName = fromMaybe "" $ Prelude.lookup "Opening" tags
    coVariation = Prelude.lookup "Variation" tags
    coPlies = Unboxed.fromList . head . concatMap (foldTree g) $ forest where
      g a [] = [[pgnPly a]]
      g a xs = (pgnPly a :) <$> fold xs

opening :: Parser Opening
opening = CO <$> lexeme code <*> lexeme var <*> pure Nothing <*> lexeme (plies startpos)

code :: Parser Text
code = p <?> "code" where
  p = f <$> alphaNumChar <*> many digitChar <*> optional alphaNumChar
  f x xs y = let s = x : xs in T.pack . fmap (chr . fromEnum) $ case y of
    Nothing -> s
    Just y' -> s ++ [y']

var :: Parser Text
var = p <?> "string" where
  p = fmap (T.pack . fmap (chr . fromEnum)) $ single quoteChar *> many ch <* single quoteChar
  ch = single backslashChar *> (  single backslashChar $> backslashChar
                              <|> single quoteChar $> quoteChar
                               )
    <|> anySingleBut quoteChar

plies :: Position -> Parser (Unboxed.Vector Ply)
plies = fmap Unboxed.fromList . go where
  go p = eol <|> line where
    eol = lexeme (string "*") $> []
    line = ply >>= \pl -> (pl :) <$> go (unsafeDoPly p pl)
    ply = validateMoveNumber p *> lexeme (relaxedSAN p)
  validateMoveNumber p =
    optional (lexeme $ L.decimal <* space <* many (single periodChar)) >>= \case
      Just n | moveNumber p /= n ->
        fail $ "Invalid move number: " <> show n <> " /= " <> show (moveNumber p)
      _ -> pure ()

-- | A parser for opening databases in SCID .eco format
scid :: Parser ECO
scid = fromList <$> scid'

scid' :: Parser [Opening]
scid' = spaceConsumer *> many opening <* eof

readSCIDECOFile :: MonadIO m => FilePath -> m (Either String ECO)
readSCIDECOFile fp = fmap fromList <$> scid_eco fp

-- | Retrieve the opening for a particular position
lookup :: Position -> ECO -> Maybe Opening
lookup pos = HashMap.lookup pos . toHashMap

type Parser = Parsec Void ByteString

spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1 (L.skipLineComment "#") (L.skipBlockComment "{" "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

periodChar, quoteChar, backslashChar :: Word8
periodChar    = fromIntegral $ ord '.'
quoteChar     = fromIntegral $ ord '"'
backslashChar = fromIntegral $ ord '\\'
