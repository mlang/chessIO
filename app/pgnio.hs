module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Text.Prettyprint.Doc.Render.Text
import Game.Chess.PGN
import System.Environment
import Text.Megaparsec

main :: IO ()
main = getArgs >>= \case
  [fp] -> do
    result <- readPGNFile fp
    case result of
      Right games -> putDoc (pgnDoc breadthFirst games)
      Left e -> putStr e
  _ -> putStrLn "Specify PGN file as argument"
