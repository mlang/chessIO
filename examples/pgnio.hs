module Main where

import Game.Chess.PGN
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = getArgs >>= readPGNFiles >>= \case
  Right pgn -> do
    hPutPGN stdout breadthFirst pgn
    exitSuccess
  Left err -> do
    hPutStr stderr err
    exitWith $ ExitFailure 1

readPGNFiles :: [FilePath] -> IO (Either String PGN)
readPGNFiles = fmap (fmap mconcat . sequenceA) . traverse readPGNFile
