module Game.Chess.UCI (
  -- * Exceptions
  UCIException(..)
  -- * The Engine data type
, Engine, name, author
  -- * Starting a UCI engine
, start, start'
  -- * Engine options
, Option(..), options, getOption, setOptionSpinButton, setOptionString
  -- * Manipulating the current game information
, isready
, currentPosition, setPosition, addPly
  -- * The Info data type
, Info(..), Score(..), Bounds(..)
  -- * Searching
, search, searching
, SearchParam
, searchmoves, timeleft, timeincrement, movestogo, movetime, nodes, depth, infinite
, stop
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
, bestMoveChan :: TChan (Ply, Maybe Ply)
, game :: IORef (Position, [Ply])
}

-- | Set the starting position of the current game, also clearing any
-- pre-existing history.
setPosition :: MonadIO m
            => Engine -> Position
            -> m (Position, [Ply])
              -- ^ the game previously in progress
setPosition e@Engine{game} p = liftIO $ do
  oldGame <- atomicModifyIORef' game ((p, []),)
  sendPosition e
  pure oldGame

data UCIException = IllegalMove Ply deriving Show

instance Exception UCIException

data Command = Name ByteString
             | Author ByteString
             | Option ByteString Option
             | UCIOk
             | ReadyOK
             | Info [Info]
             | BestMove !(Ply, Maybe Ply)
             deriving (Show)

data Info = PV [Ply]
          | Depth Int
          | SelDepth Int
          | Elapsed (Time Millisecond)
          | MultiPV Int
          | Score Score (Maybe Bounds)
          | Nodes Int
          | NPS Int
          | TBHits Int
          | HashFull Int
          | CurrMove Ply
          | CurrMoveNumber Int
          | String ByteString
          deriving (Eq, Show)

data Score = CentiPawns Int
           | MateIn Int
           deriving (Eq, Ord, Show)
           
data Bounds = UpperBound | LowerBound deriving (Eq, Show)


data Option = CheckBox Bool
            | ComboBox { comboBoxValue :: ByteString, comboBoxValues :: [ByteString] }
            | SpinButton { spinButtonValue, spinButtonMinBound, spinButtonMaxBound :: Int }
            | OString ByteString
            | Button
            deriving (Eq, Show)

instance IsString Option where
  fromString = OString . BS.pack

command :: Position -> Parser Command
command pos = skipSpace *> choice
  [ "id" `kv` name
  , "id" `kv` author
  , "option" `kv` option
  , "uciok" $> UCIOk
  , "readyok" $> ReadyOK
  , "info" `kv` fmap Info (sepBy1 infoItem skipSpace)
  , "bestmove" `kv` bestmove
  ] <* skipSpace
 where
  name = Name <$> kv "name" takeByteString
  author = Author <$> kv "author" takeByteString
  option = do
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
  str = fmap OString $
    "string" *> skipSpace *> "default" *> skipSpace *> takeByteString
  button = "button" $> Button
  infoItem = Depth <$> kv "depth" decimal
         <|> SelDepth <$> kv "seldepth" decimal
         <|> MultiPV <$> kv "multipv" decimal
         <|> kv "score" score
         <|> Nodes <$> kv "nodes" decimal
         <|> NPS <$> kv "nps" decimal
         <|> HashFull <$> kv "hashfull" decimal
         <|> TBHits <$> kv "tbhits" decimal
         <|> Elapsed . ms . fromIntegral <$> kv "time" decimal
         <|> kv "pv" pv
         <|> kv "currmove" currmove
         <|> CurrMoveNumber <$> kv "currmovenumber" decimal
         <|> String <$> kv "string" takeByteString
  score = do
    s <- kv "cp" (CentiPawns <$> signed decimal)
     <|> kv "mate" (MateIn <$> signed decimal)
    b <- optional $ skipSpace *> (  UpperBound <$ "upperbound"
                                <|> LowerBound <$ "lowerbound"
                                 )
    pure $ Score s b
  pv = fmap (PV . snd) $ foldM toPly (pos, []) =<< sepBy mv skipSpace
  toPly (pos, xs) s = case fromUCI pos s of
    Just m -> pure (doPly pos m, xs <> [m])
    Nothing -> fail $ "Failed to parse move " <> s
  currmove = fmap (fromUCI pos) mv >>= \case
    Just m -> pure $ CurrMove m
    Nothing -> fail "Failed to parse move"

  mv = BS.unpack . fst <$> match (sq *> sq *> optional (satisfy p)) where
    sq = satisfy (inRange ('a', 'h')) *> satisfy (inRange ('1', '8'))
    p 'q' = True
    p 'r' = True
    p 'b' = True
    p 'n' = True
    p _ = False 
  bestmove = do
    m <- mv
    ponder <- optional (skipSpace *> kv "ponder" mv)
    case fromUCI pos m of
      Just m' -> case ponder of
        Nothing -> pure $ BestMove (m', Nothing)
        Just p -> case fromUCI (doPly pos m') p of
          Just p' -> pure $ BestMove (m', Just p')
          Nothing -> fail $ "Failed to parse ponder move " <> p
      Nothing -> fail $ "Failed to parse best move " <> m
  kv k v = k *> skipSpace *> v

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
    Left _ -> do
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
    Left err -> outputStrLn $ err <> ":" <> show l
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

data SearchParam = SearchMoves [Ply]
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
 
searchmoves :: [Ply] -> SearchParam
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

searching :: MonadIO m => Engine -> m Bool
searching Engine{isSearching} = liftIO $ readIORef isSearching

-- | Instruct the engine to begin searching.
search :: MonadIO m
       => Engine -> [SearchParam]
       -> m (TChan (Ply, Maybe Ply), TChan [Info])
search e@Engine{isSearching} params = liftIO $ do
  chans <- atomically $ (,) <$> dupTChan (bestMoveChan e)
                            <*> dupTChan (infoChan e)
  send e . fold . intersperse " " $ "go" : foldr build mempty params
  writeIORef isSearching True
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
  set val opt@SpinButton{} = Just $ opt { spinButtonValue = val }

setOptionString :: MonadIO m => ByteString -> ByteString -> Engine -> m Engine
setOptionString n v e = liftIO $ do
  send e $ "setoption name " <> byteString n <> " value " <> byteString v
  pure $ e { options = HashMap.update (set v) n $ options e }
 where
  set val _ = Just $ OString val

-- | Return the final position of the currently active game.
currentPosition :: MonadIO m => Engine -> m Position
currentPosition Engine{game} = liftIO $
  uncurry (foldl' doPly) <$> readIORef game

nextMove :: Engine -> IO Color
nextMove Engine{game} = do
  (initialPosition, history) <- readIORef game
  pure $ if even . length $ history then color initialPosition else opponent . color $ initialPosition

-- | Add a 'Move' to the game history.
--
-- This function checks if the move is actually legal, and throws a 'UCIException'
-- if it isn't.
addPly :: MonadIO m => Engine -> Ply -> m ()
addPly e@Engine{game} m = liftIO $ do
  pos <- currentPosition e
  if m `notElem` legalPlies pos then throwIO $ IllegalMove m else do
    atomicModifyIORef' game $ \g -> (fmap (<> [m]) g, ())
    sendPosition e
 
sendPosition :: Engine -> IO ()
sendPosition e@Engine{game} = readIORef game >>= send e . cmd where
  cmd (p, h) = fold . intersperse " " $
    "position" : "fen" : fromString (toFEN p) : line h
  line [] = []
  line h = "moves" : (fromString . toUCI <$> h)

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
