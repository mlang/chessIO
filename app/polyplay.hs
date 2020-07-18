module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Random
import Data.IORef
import Data.List
import Data.String
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Time.Clock
import Data.Tree
import Game.Chess
import Game.Chess.PGN
import Game.Chess.Polyglot.Book
import Game.Chess.UCI
import Options.Applicative
import Time.Units

data Clock = Clock !Color !NominalDiffTime !NominalDiffTime !UTCTime

newClock :: Int -> IO Clock
newClock s = Clock White (fromIntegral s') (fromIntegral s') <$!> getCurrentTime
  where
 s' = s `div` 2

flipClock :: Clock -> IO Clock
flipClock clock = upd clock <$!> getCurrentTime where
  upd (Clock White w b t) t' = Clock Black (w - (t' `diffUTCTime` t)) b t'
  upd (Clock Black w b t) t' = Clock White w (b - (t' `diffUTCTime` t)) t'

clockRemaining :: Clock -> Color -> IO (Maybe (Time Millisecond))
clockRemaining (Clock c w b t) c'
  | c == c' = getCurrentTime >>= \t' -> pure $ case c of
    White -> f $ w - (t' `diffUTCTime` t)
    Black -> f $ b - (t' `diffUTCTime` t)
  | otherwise = pure $ case c' of
    White -> f w
    Black -> f b
 where
  f x | x <= 0    = Nothing
      | otherwise = Just . ms . fromRational . toRational $ x * 1000

clockTimes :: Clock -> (Maybe (Time Millisecond), Maybe (Time Millisecond))
clockTimes (Clock _ w b _) = (f w, f b) where
  f x = if x <= 0 then Nothing else Just . ms . fromRational . toRational $ x * 1000

data Polyplay = Polyplay {
  hashSize :: Int
, threadCount :: Int
, tbPath :: Maybe FilePath
, timeControl :: Int
, bookFile :: FilePath
, engineProgram :: FilePath
, engineArgs :: [String]
}

data Runtime = Runtime {
  book :: PolyglotBook
, engine :: Engine
, clock :: !Clock
}

opts :: Parser Polyplay
opts = Polyplay <$> option auto (long "hash" <> metavar "MB" <> value 1024)
                <*> option auto (long "threads" <> metavar "N" <> value 1)
                <*> optional (strOption $ long "tbpath" <> metavar "PATH")
                <*> option auto (long "time" <> metavar "SECONDS" <> value 600)
                <*> argument str (metavar "BOOK")
                <*> argument str (metavar "ENGINE")
                <*> many (argument str (metavar "ARG"))

main :: IO ()
main = run polyplay =<< execParser (info (opts <**> helper) mempty)

run :: (Runtime -> IO ()) -> Polyplay -> IO ()
run f Polyplay{..} = do
  book <- readPolyglotFile bookFile
  start engineProgram engineArgs >>= \case
    Nothing -> putStrLn "Engine failed to start."
    Just engine -> do
      _ <- setOptionSpinButton "Hash" hashSize engine
      _ <- setOptionSpinButton "Threads" threadCount engine
      case tbPath of
        Just fp -> void $ setOptionString "SyzygyPath" (fromString fp) engine
        Nothing -> pure ()
      isready engine
      clock <- newClock timeControl
      f Runtime { book, engine, clock }

polyplay :: Runtime -> IO ()
polyplay rt = do
  (h, o) <- play rt
  let g = gameFromForest [ ("White", "Stockfish")
                         , ("Black", "Stockfish")
                         ] (toForest h) o
  putDoc (gameDoc breadthFirst g)
  pure ()

play :: Runtime -> IO ([Ply], Outcome)
play Runtime{..} = do
  pos <- currentPosition engine
  case legalPlies pos of
    [] -> lost engine
    _ -> case bookPly book pos of
      Nothing -> do
        let (Just wt, Just bt) = clockTimes clock
        (bmc, ic) <- search engine [timeleft White wt, timeleft Black bt]
        sc <- newIORef Nothing
        itid <- liftIO . forkIO . forever $ do
          i <- atomically . readTChan $ ic
          case find isScore i of
            Just (Score s Nothing) -> writeIORef sc (Just s)
            _ -> pure ()
        (bm, _) <- atomically . readTChan $ bmc
        killThread itid
        clock' <- flipClock clock
        clockRemaining clock' (color pos) >>= \case
          Nothing -> lost engine
          Just _ -> do
            addPly engine bm
            s <- readIORef sc
            putStrLn $ toSAN pos bm <> " " <> show s
            play Runtime { book, engine, clock = clock'}
      Just r -> do
        pl <- evalRandIO r
        putStrLn $ toSAN pos pl
        addPly engine pl
        clock' <- flipClock clock
        play Runtime { book, engine, clock = clock' }

lost :: Engine -> IO ([Ply], Outcome)
lost e = do
  pos <- currentPosition e
  (_, h) <- setPosition e startpos
  pure (h, Win . opponent . color $ pos)

toForest :: [Ply] -> Forest Ply
toForest [] = []
toForest (x:xs) = [Node x $ toForest xs]

isScore :: Info -> Bool
isScore Score{} = True
isScore _ = False
