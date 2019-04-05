module Main where

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Extra hiding (loop)
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.IORef
import Data.List
import Data.List.Extra
import Game.Chess
import Game.Chess.UCI
import System.Console.Haskeline hiding (catch, handle)
import System.Exit
import System.Environment

data S = S {
  engine :: Engine
, mover :: Maybe ThreadId
, hintRef :: IORef (Maybe Move)
}

main :: IO ()
main = getArgs >>= \case
  [] -> do
    putStrLn "Please specify a UCI engine at the command line"
    exitWith $ ExitFailure 1
  (cmd:args) -> start cmd args >>= \case
    Nothing -> do
      putStrLn "Unable to initialise engine, maybe it doesn't speak UCI?"
      exitWith $ ExitFailure 2
    Just e -> do
      s <- S e Nothing <$> newIORef Nothing
      runInputT (setComplete (completeSAN e) defaultSettings) chessIO `evalStateT` s
      exitSuccess

completeSAN :: MonadIO m => Engine -> CompletionFunc m
completeSAN e = completeWord Nothing "" $ \w ->
  fmap (map mkCompletion . filter (w `isPrefixOf`)) $ do
    pos <- currentPosition e
    pure $ unsafeToSAN pos <$> moves pos
 where
  mkCompletion s = (simpleCompletion s) { isFinished = False }

chessIO :: InputT (StateT S IO) ()
chessIO = do
  outputStr . unlines $ [
      ""
    , "Enter a FEN string to set the starting position."
    , "To make a move, enter a SAN or UCI string."
    , "Type \"hint\" to ask for a suggestion."
    , "Type \"pass\" to let the engine make the next move, \"stop\" to end the search."
    , "Empty input will redraw the board."
    , "Hit Ctrl-D to quit."
    , ""
    ]
  outputBoard
  loop
  lift (gets engine) >>= void . quit

outputBoard :: InputT (StateT S IO) ()
outputBoard = do
  e <- lift $ gets engine
  liftIO $ do
    pos <- currentPosition e
    printBoard putStrLn pos

loop :: InputT (StateT S IO) ()
loop = do
  e <- lift $ gets engine
  getInputLine "> " >>= \case
    Nothing -> pure ()
    Just input
      | null input -> outputBoard *> loop
      | Just position <- fromFEN input -> do
        setPosition e position
        outputBoard
        loop
      | "hint" == input -> do
        lift (gets hintRef) >>= liftIO . readIORef >>= \case
          Just hint -> do
            pos <- currentPosition e
            outputStrLn $ "Try " <> toSAN pos hint
          Nothing -> outputStrLn "Sorry, no hint available"
        loop
      | "pass" == input -> do
        unlessM (isThinking e) $ do
          (bmc, _) <- go e []
          hr <- lift $ gets hintRef
          externalPrint <- getExternalPrint
          tid <- liftIO . forkIO $ doBestMove externalPrint hr bmc e
          lift $ modify' $ \s -> s { mover = Just tid }
        loop
      | input `elem` ["analyze", "analyse"] -> do
        unlessM (isThinking e) $ do
          pos <- currentPosition e
          (bmc, ic) <- go e [Infinite]
          externalPrint <- getExternalPrint
          itid <- liftIO . forkIO . forever $ do
            info <- atomically . readTChan $ ic
            case (find isScore &&& find isPV) info of
              (Just (Score cp), Just (PV pv))
                | LowerBound `notElem` info && UpperBound `notElem` info ->
                  externalPrint $ show cp <> ": " <> varToString pos pv
              _ -> pure ()
          tid <- liftIO . forkIO $ do
            (bm, ponder) <- atomically . readTChan $ bmc
            killThread itid
            pos <- currentPosition e
            externalPrint $ "Best move: " <> toSAN pos bm
          lift $ modify' $ \s -> s { mover = Just tid }
        loop
      | "stop" == input -> do
        stop e
        loop
      | otherwise -> do
        pos <- currentPosition e
        case parseMove pos input of
          Left err -> outputStrLn err
          Right m -> do
            addMove e m
            outputBoard
            (bmc, _) <- go e []
            hr <- lift $ gets hintRef
            externalPrint <- getExternalPrint
            tid <- liftIO . forkIO $ doBestMove externalPrint hr bmc e
            lift $ modify' $ \s -> s { mover = Just tid }
        loop

varToSANList :: Position -> [Move] -> [String]
varToSANList pos = snd . mapAccumL (curry (uncurry applyMove &&& uncurry toSAN)) pos

varToString :: Position -> [Move] -> String
varToString _ [] = ""
varToString pos ms
  | color pos == Black && length ms == 1
  = show (moveNumber pos) <> "..." <> toSAN pos (head ms)
  | color pos == Black
  = show (moveNumber pos) <> "..." <> toSAN pos (head ms) <> " " <> fromWhite (applyMove pos (head ms)) (tail ms)
  | otherwise
  = fromWhite pos ms
 where
  fromWhite pos ms = unwords . concat . zipWith f [moveNumber pos ..] . chunksOf 2 $ varToSANList pos ms
  f n (x:xs) = (show n <> "." <> x):xs

parseMove :: Position -> String -> Either String Move
parseMove pos s = case fromUCI pos s of
  Just m -> pure m
  Nothing -> fromSAN pos s

printBoard :: (String -> IO ()) -> Position -> IO ()
printBoard externalPrint pos = externalPrint . init . unlines $
  (map . map) pc (reverse $ chunksOf 8 [A1 .. H8])
 where
  pc sq = (if isDark sq then toUpper else toLower) $ case pieceAt pos sq of
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

doBestMove :: (String -> IO ())
           -> IORef (Maybe Move)
           -> TChan (Move, Maybe Move)
           -> Engine
           -> IO ()
doBestMove externalPrint hintRef bmc e = do
  (bm, ponder) <- atomically . readTChan $ bmc
  pos <- currentPosition e
  externalPrint $ "< " <> toSAN pos bm
  addMove e bm
  currentPosition e >>= printBoard externalPrint
  writeIORef hintRef ponder

printPV :: (String -> IO ()) -> TChan [Info] -> Engine -> IO ()
printPV externalPrint ic engine = forever $ do
  info <- atomically . readTChan $ ic
  case find isPV info of
    Just pv -> externalPrint $ show pv
    Nothing -> pure ()

isPV, isScore :: Info -> Bool
isPV PV{}       = True
isPV _          = False
isScore Score{} = True
isScore _       = False
