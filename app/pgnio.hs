module Main where

import Game.Chess.PGN
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec

main :: IO ()
main = getArgs >>= \case
  [fp] -> readPGNFile fp >>= \case
    Right pgn -> do
      hPutPGN stdout breadthFirst pgn
      exitSuccess
    Left err -> do
      hPutStr stderr err
      exitWith (ExitFailure 1)
  _ -> hPutStrLn stderr "Specify PGN file as argument"
