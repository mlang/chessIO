module Main where

import Control.Arrow ((&&&))
import Control.Concurrent ( forkIO, killThread, ThreadId )
import Control.Concurrent.STM ( atomically, readTChan, TChan )
import Control.Monad ( forever, void )
import Control.Monad.Extra ( ifM, unlessM )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Random ( evalRandIO, MonadTrans(lift) )
import Control.Monad.State.Strict
    ( StateT, evalStateT, gets, modify, modify' )
import Data.Char ( toLower, toUpper )
import Data.IORef ( newIORef, readIORef, writeIORef, IORef )
import Data.List ( find, isPrefixOf )
import Data.List.Extra ( chunksOf )
import Game.Chess
    ( fromFEN, fromUCI, isDark, legalPlies, pieceAt, startpos, fromSAN, toSAN,
      unsafeToSAN, Color(Black, White),
      PieceType(King, Pawn, Knight, Bishop, Rook, Queen),
      Ply, Position, Sq(H8, A1), varToSAN )
import Game.Chess.Polyglot
    ( PolyglotBook, defaultBook, readPolyglotFile, bookPlies, bookPly )
import Game.Chess.UCI
    ( addPly,
      currentPosition,
      infinite,
      movetime,
      quit,
      search,
      searching,
      setPosition,
      start,
      stop,
      Engine,
      BestMove, Info(Score, PV) )
import System.Console.Haskeline
    ( defaultSettings,
      getExternalPrint,
      getInputLine,
      outputStr,
      outputStrLn,
      completeWord,
      simpleCompletion,
      runInputT,
      setComplete,
      Completion(isFinished),
      CompletionFunc,
      InputT )
import System.Exit ( ExitCode(ExitFailure), exitSuccess, exitWith )
import System.Environment ( getArgs )
import Time.Units ( ms, sec )

data S = S {
  engine :: Engine
, mover :: Maybe ThreadId
, book :: PolyglotBook
, hintRef :: IORef (Maybe Ply)
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
      s <- S e Nothing defaultBook <$> newIORef Nothing
      runInputT (setComplete (completeSAN e) defaultSettings) chessIO `evalStateT` s
      exitSuccess

completeSAN :: MonadIO m => Engine -> CompletionFunc m
completeSAN e = completeWord Nothing "" $ \w ->
  fmap (map mkCompletion . filter (w `isPrefixOf`)) $ do
    pos <- currentPosition e
    pure $ unsafeToSAN pos <$> legalPlies pos
 where
  mkCompletion s = (simpleCompletion s) { isFinished = False }

chessIO :: InputT (StateT S IO) ()
chessIO = do
  outputStr . unlines $ [
      ""
    , "Enter a FEN string to set the starting position."
    , "To make a move, enter a SAN or UCI string."
    , "Type \"hint\" to ask for a suggestion."
    , "Type \"pass\" to let the engine make the next move,"
    , "     \"analyse\" to watch the engine ponder the current position and"
    , "     \"stop\" to end the search."
    , "Empty input will redraw the board."
    , "Hit Ctrl-D to quit."
    , ""
    ]
  outputBoard
  loop
  lift (gets engine) >>= void . quit

midgame :: InputT (StateT S IO) ()
midgame = do
  e <- lift $ gets engine
  b <- lift $ gets book
  pos <- currentPosition e
  case bookPly b pos of
    Just r -> do
      pl <- liftIO . evalRandIO $ r
      addPly e pl
      (bmc, _) <- search e [movetime (ms 100)]
      bm <- liftIO . atomically . readTChan $ bmc
      case bm of
        Just (bm', _) -> do
          addPly e bm'
          midgame
        Nothing -> outputBoard
    Nothing -> outputBoard

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
        void $ setPosition e position
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
        unlessM (searching e) $ do
          (bmc, _) <- search e [movetime (sec 2)]
          hr <- lift $ gets hintRef
          externalPrint <- getExternalPrint
          tid <- liftIO . forkIO $ doBestMove externalPrint hr bmc e
          lift $ modify' $ \s -> s { mover = Just tid }
        loop
      | input `elem` ["analyze", "analyse"] -> do
        unlessM (searching e) $ do
          pos <- currentPosition e
          (bmc, ic) <- search e [infinite]
          externalPrint <- getExternalPrint
          itid <- liftIO . forkIO . forever $ do
            info <- atomically . readTChan $ ic
            case (find isScore &&& find isPV) info of
              (Just (Score s Nothing), Just (PV pv)) ->
                externalPrint $ show s <> ": " <> varToSAN pos pv
              _ -> pure ()
          tid <- liftIO . forkIO $ do
            bm <- atomically . readTChan $ bmc
            killThread itid
            case bm of
              Just (bm', _) -> externalPrint $ "Best move: " <> toSAN pos bm'
              Nothing -> pure ()
          lift $ modify' $ \s -> s { mover = Just tid }
        loop
      | "stop" == input -> do
        stop e
        loop
      | ["polyglot", file] <- words input -> do
        b <- liftIO $ readPolyglotFile file
        lift $ modify $ \x -> x { book = b }
        loop
      | "book" == input -> do
        b <- lift $ gets book
        pos <- currentPosition e
        let plies = bookPlies b pos
        if not . null $ plies
          then do
            addPly e (head plies)
            outputBoard
            searchBestMove
          else pure ()
        loop
      | "midgame" == input -> do
        void $ setPosition e startpos
        midgame
        loop
      | otherwise -> do
        pos <- currentPosition e
        case parseMove pos input of
          Left err -> outputStrLn err
          Right m -> ifM (searching e) (outputStrLn "Not your move") $ do
            addPly e m
            outputBoard
            searchBestMove
        loop

searchBestMove :: InputT (StateT S IO) ()
searchBestMove = do
  e <- lift $ gets engine
  (bmc, _) <- search e [movetime (sec 1)]
  hr <- lift $ gets hintRef
  externalPrint <- getExternalPrint
  tid <- liftIO . forkIO $ doBestMove externalPrint hr bmc e
  lift $ modify' $ \s -> s { mover = Just tid }

parseMove :: Position -> String -> Either String Ply
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
           -> IORef (Maybe Ply)
           -> TChan BestMove
           -> Engine
           -> IO ()
doBestMove externalPrint hintRef bmc e = do
  bm <- atomically . readTChan $ bmc
  case bm of
    Just (bm', pndr) -> do
      pos <- currentPosition e
      externalPrint $ "< " <> toSAN pos bm'
      addPly e bm'
      currentPosition e >>= printBoard externalPrint
      writeIORef hintRef pndr
    Nothing -> pure ()

printPV :: (String -> IO ()) -> TChan [Info] -> IO ()
printPV externalPrint ic = forever $ do
  info <- atomically . readTChan $ ic
  case find isPV info of
    Just pv -> externalPrint $ show pv
    Nothing -> pure ()

isPV, isScore :: Info -> Bool
isPV PV{}       = True
isPV _          = False
isScore Score{} = True
isScore _       = False
