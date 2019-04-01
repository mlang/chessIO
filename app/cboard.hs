module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Char
import Data.IORef
import Data.List
import Data.List.Split
import Game.Chess
import qualified Game.Chess.UCI as UCI
import System.Console.Haskeline hiding (catch, handle)
import System.Exit
import System.Environment

data S = S {
  engine :: UCI.Engine
, mover :: Maybe ThreadId
, hintRef :: IORef (Maybe Move)
}

main = getArgs >>= \case
  [] -> do
    putStrLn "Please specify a UCI engine at the command line"
    exitWith $ ExitFailure 1
  (cmd:args) -> do
    UCI.start cmd args >>= \case
      Nothing -> do
        putStrLn "Unable to initialise engine, maybe it doesn't speak UCI?"
        exitWith $ ExitFailure 2
      Just e -> do
        s <- S e Nothing <$> newIORef Nothing
        runInputT (setComplete (completeSAN e) defaultSettings) chessIO `evalStateT` s
        exitWith ExitSuccess

completeSAN :: MonadIO m => UCI.Engine -> CompletionFunc m
completeSAN e = completeWord Nothing "" $ \w ->
  fmap (map mkCompletion . filter (w `isPrefixOf`)) $ do
    pos <- liftIO $ UCI.currentPosition e
    pure $ map toUCI $ moves pos
 where
  mkCompletion s = (simpleCompletion s) { isFinished = False }

chessIO :: InputT (StateT S IO) ()
chessIO = do
  externalPrint <- getExternalPrint
  e <- lift $ gets engine
  hr <- lift $ gets hintRef
  tid <- liftIO . forkIO $ doBestMove externalPrint hr e
  lift $ modify' $ \s -> s { mover = Just tid }
  outputBoard
  loop
  void . liftIO $ UCI.quit e

outputBoard :: InputT (StateT S IO) ()
outputBoard = do
  e <- lift $ gets engine
  liftIO $ do
    pos <- UCI.currentPosition e
    printBoard putStrLn pos

loop :: InputT (StateT S IO) ()
loop = do
  e <- lift $ gets engine
  getInputLine "> " >>= \case
    Nothing -> pure ()
    Just input
      | null input -> loop
      | Just position <- fromFEN input -> do
        outputStrLn $ toFEN position
        loop
      | input == "hint" -> do
        lift (gets hintRef) >>= liftIO . readIORef >>= \case
          Just hint -> outputStrLn $ "Try " <> show hint
          Nothing -> outputStrLn "Sorry, no hint available"
        loop
      | otherwise -> do
        liftIO $ do
          printUCIException `handle` do
            UCI.move e input
            UCI.send "go movetime 1000" e
        outputBoard
        loop

printBoard :: (String -> IO ()) -> Position -> IO ()
printBoard externalPrint pos = externalPrint . init . unlines $
  (map . map) pc (reverse $ chunksOf 8 [A1 .. H8])
 where
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
doBestMove :: (String -> IO ()) -> IORef (Maybe Move) -> UCI.Engine -> IO ()
doBestMove externalPrint hintRef e = forever $ do
  (bm, ponder) <- atomically . UCI.readBestMove $ e
  UCI.addMove e bm
  externalPrint $ "< " <> show bm
  UCI.currentPosition e >>= printBoard externalPrint
  writeIORef hintRef ponder

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
printUCIException (UCI.SANError e) = putStrLn e
printUCIException (UCI.IllegalMove m) = putStrLn $ "Illegal move: " <> show m
