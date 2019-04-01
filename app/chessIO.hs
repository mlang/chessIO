module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.IORef
import Data.List
import Data.List.Split
import Game.Chess
import qualified Game.Chess.UCI as UCI
import System.Console.Haskeline hiding (catch, handle)
import System.Exit
import System.Environment

main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Please specify a UCI engine at the command line"
      exitWith $ ExitFailure 1
    (cmd:args) -> exitWith =<< runInputT defaultSettings (chessIO cmd args)

chessIO :: String -> [String] -> InputT IO ExitCode
chessIO cmd args = do
  externalPrint <- getExternalPrint
  me <- liftIO $ UCI.start' 1000000 externalPrint cmd args
  case me of
    Nothing -> do
      outputStrLn "Unable to initialise engine, maybe it doesn't speak UCI?"
      pure $ ExitFailure 2
    Just engine -> do
      liftIO . forkIO $ printPV externalPrint engine
      loop engine
      void . liftIO $ UCI.quit engine
      pure ExitSuccess

loop :: UCI.Engine -> InputT IO ()
loop engine = do
  liftIO $ do
    pos <- UCI.currentPosition engine
    printBoard pos
  minput <- getInputLine "> "
  case minput of
    Nothing -> pure ()
    Just input
      | null input -> loop engine
      | Just position <- fromFEN input -> do
        outputStrLn $ toFEN position
        loop engine
      | otherwise -> do
        liftIO $ do
          printUCIException `handle` do
            UCI.move input engine
            UCI.send "go wtime 10000 btime 10000" engine
        loop engine

printBoard :: Position -> IO ()
printBoard pos = putStr . unlines $ (map . map) pc (reverse $ chunksOf 8 [A1 .. H8]) where
  pc sq = (if isDark sq then toUpper else toLower) case pieceAt pos sq of
    Just (White, Pawn)   -> 'P'
    Just (White, Knight) -> 'N'
    Just (White, Bishop) -> 'B'
    Just (White, Rook)   -> 'R'
    Just (White, Queen)  -> 'Q'
    Just (White, King)   -> 'K'
    Just (Black, Pawn)   -> 'X'
    Just (Black, Knight) -> 'S'
    Just (Black, Bishop) -> 'L'
    Just (Black, Rook)   -> 'T'
    Just (Black, Queen)  -> 'D'
    Just (Black, King)   -> 'J'
    Nothing | isDark sq -> '.'
            | otherwise -> ' '
printPV :: (String -> IO ()) -> UCI.Engine -> IO ()
printPV externalPrint engine = forever $ do
  info <- atomically . UCI.readInfo $ engine
  case find isPV info of
    Just pv -> externalPrint $ show pv
    Nothing -> pure ()
 where
  isPV UCI.PV{} = True
  isPV _        = False

printUCIException :: UCI.UCIException -> IO ()
printUCIException e = print e
