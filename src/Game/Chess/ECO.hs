{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.Chess.ECO (Opening(..), ECO, defaultECO, readSCIDECOFile, lookup) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Bifunctor
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Either (fromRight)
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Void
import Game.Chess
import Game.Chess.SAN
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

defaultECO :: ECO
defaultECO = case parse eco "scid.eco" $(embedFile "book/scid.eco") of
  Right x -> x

data Opening = CO {
  _code :: Text
, _name :: Text
, _plies :: [Ply]
} deriving (Show)

type ECO = HashMap Position Opening

opening :: Parser Opening
opening = CO <$> lexeme code <*> lexeme str <*> lexeme (plies startpos)

code :: Parser Text
code = f <$> alphaNumChar <*> many digitChar <*> optional alphaNumChar where
  f x xs y = let s = x : xs in T.pack . fmap (chr . fromEnum) $ case y of
    Nothing -> s
    Just y' -> s ++ [y']

str :: Parser Text
str = p <?> "string" where
  p = fmap (T.pack . fmap (chr . fromEnum)) $ single quoteChar *> many ch <* single quoteChar
  ch = single backslashChar *> (  single backslashChar $> backslashChar
                              <|> single quoteChar $> quoteChar
                               )
    <|> anySingleBut quoteChar

plies :: Position -> Parser [Ply]
plies p = eol <|> line where
  eol = lexeme (string "*") $> []
  line = ply >>= \pl -> (pl :) <$> plies (unsafeDoPly p pl)
  ply = validateMoveNumber p *> lexeme (relaxedSAN p)
  validateMoveNumber p =
    optional (lexeme $ L.decimal <* space <* many (single periodChar)) >>= \case
      Just n | moveNumber p /= n ->
        fail $ "Invalid move number: " <> show n <> " /= " <> show (moveNumber p)
      _ -> pure ()

eco :: Parser ECO
eco = fmap hm $ spaceConsumer *> many opening <* eof

readSCIDECOFile :: FilePath -> IO (Either String ECO)
readSCIDECOFile fp = first errorBundlePretty . parse eco fp <$> BS.readFile fp

lookup :: Position -> ECO -> Maybe Opening
lookup = HashMap.lookup

hm :: [Opening] -> HashMap Position Opening
hm = HashMap.fromList . fmap f where
  f co = (foldl' doPly startpos $ _plies co, co)

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
