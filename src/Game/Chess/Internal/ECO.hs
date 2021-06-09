{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.Chess.Internal.ECO where

import Prelude hiding (lookup)
import qualified Prelude as Prelude
import Control.DeepSeq
import Control.Monad
import Data.Bifunctor
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.Binary
import Data.Char
import Data.Data
import Data.Either (fromRight)
import Data.FileEmbed (embedFile)
import Data.Foldable (fold)
import Data.Functor
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.MonoTraversable (MonoFoldable(ofoldl'))
import Data.Ord
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (foldTree)
import qualified Data.Vector.Unboxed as Unboxed
import Data.Vector.Binary
import Data.Vector.Instances
import Data.Void
import Data.Word (Word8)
import Game.Chess
import Game.Chess.PGN
import Game.Chess.SAN
import Instances.TH.Lift
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L
import Language.Haskell.TH.Syntax (Lift)

import Control.Exception (Exception(displayException), throwIO)
import Control.Monad.IO.Class
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax

type FileType = FilePath -> Q (Either String (Q (TExp [Opening])))

eco_pgn, scid_eco :: FileType
eco_pgn fp = fmap (liftTyped.fromPGN') <$> readPGNFile fp
scid_eco fp = bimap errorBundlePretty liftTyped . parse scid' fp <$>
              (liftIO $ BS.readFile fp)

embedECO :: FileType -> FilePath -> Q (TExp ECO)
embedECO load fp = load fp >>= \case
  Right xs -> [|| fromList $$(xs) ||]
  Left err -> fail err

data Opening = CO {
  coCode :: !Text
, coName :: !Text
, coVariation :: !(Maybe Text)
, coPlies :: !(Unboxed.Vector Ply)
} deriving (Eq, Generic, Lift, Show)

instance Binary Opening
instance Hashable Opening
instance NFData Opening

newtype ECO = ECO { toHashMap :: HashMap Position Opening }
              deriving (Eq, NFData, Hashable, Semigroup, Monoid)

toList :: ECO -> [Opening]
toList = map snd . HashMap.toList . toHashMap

fromList :: [Opening] -> ECO
fromList = ECO . HashMap.fromList . fmap (\co -> (pos co, co)) where
  pos = ofoldl' unsafeDoPly startpos . coPlies

instance Binary ECO where
  put = put . toList
  get = fromList <$> get

--defaultECO :: ECO
--defaultECO = fromRight mempty $
--  fromPGN <$> parse pgn "book/eco.pgn" $(embedFile "book/eco.pgn")

scidECO :: ECO
scidECO = fromRight mempty $ parse scid "book/scid.eco" $(embedFile "book/scid.eco")

fromPGN :: PGN -> ECO
fromPGN = fromList . fromPGN'

fromPGN' :: PGN -> [Opening]
fromPGN' (PGN games) = map mkCO games where
  mkCO (tags, (_, forest)) = CO { .. } where
    coCode = maybe "" id $ Prelude.lookup "ECO" tags
    coName = maybe "" id $ Prelude.lookup "Opening" tags
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

scid :: Parser ECO
scid = fromList <$> scid'

scid' :: Parser [Opening]
scid' = spaceConsumer *> many opening <* eof

readSCIDECOFile :: FilePath -> IO (Either String ECO)
readSCIDECOFile fp = first errorBundlePretty . parse scid fp <$> BS.readFile fp

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
