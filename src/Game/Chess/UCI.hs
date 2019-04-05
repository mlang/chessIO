module Game.Chess.UCI (
  -- * Exceptions
  UCIException(..)
  -- * The Engine data type
, Engine, name, author
  -- * Starting a UCI engine
, start, start'
  -- * Engine options
, Option(..), options, getOption, setOptionSpinButton
  -- * Manipulating the current game information
, currentPosition, setPosition, addMove
  -- * The Info data type
, Info(..)
  -- * Searching
, SearchParam
, searchmoves, timeleft, timeincrement, movestogo, movetime, nodes, depth
, infinite
, isThinking, go, stop
  -- * Quitting
, quit, quit'
) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Builder
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Ix
import Data.List
import Data.Maybe
import Data.String (IsString(..))
import Game.Chess
import Numeric.Natural
import System.Exit (ExitCode)
import System.IO
import System.Process
import Time.Rational
import Time.Units

data Engine = Engine {
  inH :: Handle
, outH :: Handle
, procH :: ProcessHandle
, outputStrLn :: String -> IO ()
, infoThread :: Maybe ThreadId
, name :: Maybe ByteString
, author :: Maybe ByteString
, options :: HashMap ByteString Option
, isReady :: MVar ()
, isSearching :: IORef Bool
, infoChan :: TChan [Info]
, bestMoveChan :: TChan (Move, Maybe Move)
, game :: IORef (Position, [Move])
}

-- | Set the starting position of the current game, also clearing any
-- pre-existing history.
setPosition :: MonadIO m => Engine -> Position -> m ()
setPosition e@Engine{game} p = liftIO $ do
  atomicWriteIORef game (p, [])
  sendPosition e

data UCIException = IllegalMove Move deriving Show

instance Exception UCIException

data Command = Name ByteString
             | Author ByteString
             | Option ByteString Option
             | UCIOk
             | ReadyOK
             | Info [Info]
             | BestMove !(Move, (Maybe Move))
             deriving (Show)

data Info = PV [Move]
          | Depth Int
          | SelDepth Int
          | Time Int
          | MultiPV Int
          | Score Int
          | UpperBound
          | LowerBound
          | Nodes Int
          | NPS Int
          | TBHits Int
          | HashFull Int
          | CurrMove Move
          | CurrMoveNumber Int
          deriving (Eq, Show)

data Option = CheckBox Bool
            | ComboBox { comboBoxValue :: ByteString, comboBoxValues :: [ByteString] }
            | SpinButton { spinButtonValue, spinButtonMinBound, spinButtonMaxBound :: Int }
            | String ByteString
            | Button
            deriving (Eq, Show)

instance IsString Option where
  fromString = String . BS.pack

command :: Position -> Parser Command
command pos = skipSpace *> choice [
    name, author, option, uciok, readyok, info, bestmove
  ] <* skipSpace
 where
  name = fmap Name $
    "id" *> skipSpace *> "name" *> skipSpace *> takeByteString
  author = fmap Author $
    "id" *> skipSpace *> "author" *> skipSpace *> takeByteString
  option = do
    void "option"
    skipSpace
    void "name"
    skipSpace
    optName <- BS.pack <$> manyTill anyChar (skipSpace *> "type")
    skipSpace
    optValue <- spin <|> check <|> combo <|> str <|> button
    pure $ Option optName optValue
  check =
    fmap CheckBox $ "check" *> skipSpace *> "default" *> skipSpace *>
                    ("false" $> False <|> "true" $> True)
  spin = do
    void "spin"
    skipSpace
    value <- "default" *> skipSpace *> signed decimal <* skipSpace
    minValue <- "min" *> skipSpace *> signed decimal <* skipSpace
    maxValue <- "max" *> skipSpace *> signed decimal
    pure $ SpinButton value minValue maxValue
  combo = do
    void "combo"
    skipSpace
    def <- fmap BS.pack $ "default" *> skipSpace *> manyTill anyChar var
    (vars, lastVar) <- (,) <$> many (manyTill anyChar var)
                           <*> takeByteString
    pure $ ComboBox def (map BS.pack vars <> [lastVar])
  var = skipSpace *> "var" *> skipSpace
  str = fmap String $
    "string" *> skipSpace *> "default" *> skipSpace *> takeByteString
  button = "button" $> Button
  uciok = "uciok" $> UCIOk
  readyok = "readyok" $> ReadyOK
  info = do
    "info"
    skipSpace
    Info <$> sepBy1 infoItem skipSpace
  infoItem = Depth <$> ("depth" *> skipSpace *> decimal)
         <|> SelDepth <$> ("seldepth" *> skipSpace *> decimal)
         <|> MultiPV <$> ("multipv" *> skipSpace *> decimal)
         <|> Score <$> ("score" *> skipSpace *> "cp" *> skipSpace *> signed decimal)
         <|> UpperBound <$ "upperbound"
         <|> LowerBound <$ "lowerbound"
         <|> Nodes <$> ("nodes" *> skipSpace *> decimal)
         <|> NPS <$> ("nps" *> skipSpace *> decimal)
         <|> HashFull <$> ("hashfull" *> skipSpace *> decimal)
         <|> TBHits <$> ("tbhits" *> skipSpace *> decimal)
         <|> Game.Chess.UCI.Time <$> ("time" *> skipSpace *> decimal)
         <|> pv
         <|> currMove
         <|> CurrMoveNumber <$> ("currmovenumber" *> skipSpace *> decimal)
  pv = do
    xs <- (fmap . fmap) BS.unpack $ "pv" *> skipSpace *> sepBy mv skipSpace
    PV . snd <$> foldM toMove (pos, []) xs
  toMove (pos, xs) s = do
    case fromUCI pos s of
      Just m -> pure (applyMove pos m, xs <> [m])
      Nothing -> fail $ "Failed to parse move " <> s
  currMove = do
    s <- fmap BS.unpack $ "currmove" *> skipSpace *> mv
    case fromUCI pos s of
      Just m -> pure $ CurrMove m
      Nothing -> fail $ "Failed to parse move " <> s

  mv = fmap fst $ match $ satisfy f *> satisfy r *> satisfy f *> satisfy r *> optional (satisfy p) where
    f = inRange ('a','h')
    r = inRange ('1', '8')
    p 'q' = True
    p 'r' = True
    p 'b' = True
    p 'n' = True
    p _ = False 
  bestmove = do
    void "bestmove"
    skipSpace
    m <- BS.unpack <$> mv
    ponder <- (fmap . fmap) BS.unpack $
              optional (skipSpace *> "ponder" *> skipSpace *> mv)
    case fromUCI pos m of
      Just m' -> case ponder of
        Nothing -> pure $ BestMove (m', Nothing)
        Just p -> case fromUCI (applyMove pos m') p of
          Just p' -> pure $ BestMove (m', (Just p'))
          Nothing -> fail $ "Failed to parse ponder move " <> p
      Nothing -> fail $ "Failed to parse best move " <> m

-- | Start a UCI engine with the given executable name and command line arguments.
start :: String -> [String] -> IO (Maybe Engine)
start = start' (sec 2) putStrLn

-- | Start a UCI engine with the given timeout for initialisation.
--
-- If the engine takes more then the given microseconds to answer to the
-- initialisation request, 'Nothing' is returned and the external process
-- will be terminated.
start' :: KnownDivRat unit Microsecond => Time unit -> (String -> IO ()) -> String -> [String] -> IO (Maybe Engine)
start' tout outputStrLn cmd args = do
  (Just inH, Just outH, Nothing, procH) <- createProcess (proc cmd args) {
      std_in = CreatePipe, std_out = CreatePipe
    }
  hSetBuffering inH LineBuffering
  e <- Engine inH outH procH outputStrLn Nothing Nothing Nothing HashMap.empty <$>
       newEmptyMVar <*> newIORef False <*>
       newBroadcastTChanIO <*> newBroadcastTChanIO <*>
       newIORef (startpos, [])
  send e "uci"
  timeout tout (initialise e) >>= \case
    Just e' -> do
      tid <- forkIO . infoReader $ e'
      pure . Just $ e' { infoThread = Just tid }
    Nothing -> quit e $> Nothing

initialise :: Engine -> IO Engine
initialise c@Engine{outH, outputStrLn, game} = do
  l <- BS.hGetLine outH
  pos <- fst <$> readIORef game
  if BS.null l then initialise c else case parseOnly (command pos <* endOfInput) l of
    Left err -> do
      outputStrLn . BS.unpack $ l
      initialise c
    Right (Name n) -> initialise (c { name = Just n })
    Right (Author a) -> initialise (c { author = Just a })
    Right (Option name opt) -> initialise (c { options = HashMap.insert name opt $ options c })
    Right UCIOk -> pure c

infoReader :: Engine -> IO ()
infoReader e@Engine{..} = forever $ do
  l <- BS.hGetLine outH
  pos <- currentPosition e
  case parseOnly (command pos <* endOfInput) l of
    Left err -> do
      outputStrLn $ err <> ":" <> show l
    Right ReadyOK -> putMVar isReady ()
    Right (Info i) -> atomically $ writeTChan infoChan i
    Right (BestMove bm) -> do
      writeIORef isSearching False
      atomically $ writeTChan bestMoveChan bm

-- | Wait until the engine is ready to take more commands.
isready :: Engine -> IO ()
isready e@Engine{isReady} = do
  send e "isready"
  takeMVar isReady
  
send :: Engine -> Builder -> IO ()
send Engine{inH, procH} b = do
  hPutBuilder inH (b <> "\n")
  getProcessExitCode procH >>= \case
    Nothing -> pure ()
    Just ec -> throwIO ec

data SearchParam = SearchMoves [Move]
                -- ^ restrict search to the specified moves only
                 | TimeLeft Color (Time Millisecond)
                -- ^ time (in milliseconds) left on the clock
                 | TimeIncrement Color (Time Millisecond)
                -- ^ time increment per move in milliseconds
                 | MovesToGo Natural
                -- ^ number of moves to the next time control
                 | MoveTime (Time Millisecond)
                 | MaxNodes Natural
                 | MaxDepth Natural
                 | Infinite
                -- ^ search until 'stop' gets called
                 deriving (Eq, Show)
 
searchmoves :: [Move] -> SearchParam
searchmoves = SearchMoves

timeleft, timeincrement :: KnownDivRat unit Millisecond
                        => Color -> Time unit -> SearchParam
timeleft c = TimeLeft c . toUnit
timeincrement c = TimeIncrement c . toUnit

movestogo :: Natural -> SearchParam
movestogo = MovesToGo

movetime :: KnownDivRat unit Millisecond => Time unit -> SearchParam
movetime = MoveTime . toUnit

nodes, depth :: Natural -> SearchParam
nodes = MaxNodes
depth = MaxDepth

infinite :: SearchParam
infinite = Infinite

isThinking :: MonadIO m => Engine -> m Bool
isThinking Engine{isSearching} = liftIO $ readIORef isSearching

-- | Instruct the engine to begin searching.
go :: MonadIO m
   => Engine -> [SearchParam]
   -> m (TChan (Move, Maybe Move), TChan [Info])
go e params = liftIO $ do
  chans <- atomically $ (,) <$> dupTChan (bestMoveChan e)
                            <*> dupTChan (infoChan e)
  send e . fold . intersperse " " $ "go" : foldr build mempty params
  pure chans
 where
  build (SearchMoves ms) xs = "searchmoves" : (fromString . toUCI <$> ms) <> xs
  build (TimeLeft White (floor . unTime -> x)) xs = "wtime" : integerDec x : xs
  build (TimeLeft Black (floor . unTime -> x)) xs = "btime" : integerDec x : xs
  build (TimeIncrement White (floor . unTime -> x)) xs = "winc" : integerDec x : xs
  build (TimeIncrement Black (floor . unTime -> x)) xs = "binc" : integerDec x : xs
  build (MovesToGo x) xs = "movestogo" : naturalDec x : xs
  build (MoveTime (floor . unTime -> x)) xs = "movetime" : integerDec x : xs
  build (MaxNodes x) xs = "nodes" : naturalDec x : xs
  build (MaxDepth x) xs = "depth" : naturalDec x : xs
  build Infinite xs = "infinite" : xs
  naturalDec = integerDec . toInteger

-- | Stop a search in progress.
stop :: MonadIO m => Engine -> m ()
stop e = liftIO $ send e "stop"

getOption :: ByteString -> Engine -> Maybe Option
getOption n = HashMap.lookup n . options

-- | Set a spin option to a particular value.
--
-- Bounds are validated.  Make sure you don't set a value which is out of range.
setOptionSpinButton :: MonadIO m => ByteString -> Int -> Engine -> m Engine
setOptionSpinButton n v c
  | Just (SpinButton _ minValue maxValue) <- getOption n c
  , inRange (minValue, maxValue) v
  = liftIO $ do
    send c $ "setoption name " <> byteString n <> " value " <> intDec v
    pure $ c { options = HashMap.update (set v) n $ options c }
 where
  set v opt@SpinButton{} = Just $ opt { spinButtonValue = v }

-- | Return the final position of the currently active game.
currentPosition :: MonadIO m => Engine -> m Position
currentPosition Engine{game} = liftIO $
  uncurry (foldl' applyMove) <$> readIORef game

nextMove :: Engine -> IO Color
nextMove Engine{game} = do
  (initialPosition, history) <- readIORef game
  pure $ if even . length $ history then color initialPosition else opponent . color $ initialPosition

-- | Add a 'Move' to the game history.
--
-- This function checks if the move is actually legal, and throws a 'UCIException'
-- if it isn't.
addMove :: MonadIO m => Engine -> Move -> m ()
addMove e@Engine{game} m = liftIO $ do
  pos <- currentPosition e
  if m `notElem` moves pos then throwIO $ IllegalMove m else do
    atomicModifyIORef' game \g -> (fmap (<> [m]) g, ())
    sendPosition e
 
sendPosition :: Engine -> IO ()
sendPosition e@Engine{game} = do
  readIORef game >>= send e . cmd
 where
  cmd (p, h) = "position fen " <> fromString (toFEN p) <> line h
  line h
    | null h    = ""
    | otherwise = " moves " <> fold (intersperse " " (fromString . toUCI <$> h))

-- | Quit the engine.
quit :: MonadIO m => Engine -> m (Maybe ExitCode)
quit = quit' (sec 1)

quit' :: (KnownDivRat unit Microsecond, MonadIO m)
      => Time unit -> Engine -> m (Maybe ExitCode)
quit' t e@Engine{procH, infoThread} = liftIO $ (pure . Just) `handle` do
  maybe (pure ()) killThread infoThread
  send e "quit"
  timeout t (waitForProcess procH) >>= \case
    Just ec -> pure $ Just ec
    Nothing -> terminateProcess procH $> Nothing
