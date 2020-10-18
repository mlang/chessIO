module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Random
import Data.Maybe (isJust, maybe)
import Data.IORef
import Data.List
import Data.String
import Data.Text.Encoding (decodeUtf8)
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
, history :: (Position, [Ply])
, active :: Player Active
, passive :: Player Passive
, clock :: !Clock
}

data Player s = Player Engine (Maybe s)
data Active = Searching (TChan (Ply, Maybe Ply)) (TChan [Info])
data Passive = Pondering Ply (TChan (Ply, Maybe Ply)) (TChan [Info])

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
    Just e1 -> do
      _ <- setOptionSpinButton "Hash" hashSize e1
      _ <- setOptionSpinButton "Threads" threadCount e1
      case tbPath of
        Just fp -> void $ setOptionString "SyzygyPath" (fromString fp) e1
        Nothing -> pure ()
      isready e1
      start engineProgram engineArgs >>= \case
        Nothing -> putStrLn "Engine failed to start secondary engine."
        Just e2 -> do
          _ <- setOptionSpinButton "Hash" hashSize e2
          _ <- setOptionSpinButton "Threads" threadCount e2
          case tbPath of
            Just fp -> void $ setOptionString "SyzygyPath" (fromString fp) e2
            Nothing -> pure ()
          isready e2
          let history = (startpos, [])
          let active = Player e1 Nothing
          let passive = Player e2 Nothing
          clock <- newClock timeControl
          f Runtime { book, history, active, passive, clock }

polyplay :: Runtime -> IO ()
polyplay rt = do
  (h, o) <- play rt
  let wname = maybe "Unknown" decodeUtf8 $
              case (active rt) of Player e _ -> name e
  let bname = wname
  let g = gameFromForest [ ("White", wname)
                         , ("Black", bname)
                         ] (toForest h) o
  putDoc (gameDoc breadthFirst g)
  pure ()

done :: Position -> Bool
done = null . legalPlies

play :: Runtime -> IO ([Ply], Outcome)
play rt@Runtime{book, history, active, passive, clock} = do
  let pos = uncurry (foldl' doPly) history
  clockRemaining clock (color pos) >>= \case
    Nothing -> pure (snd history, Win . opponent . color $ pos)
    Just _ ->
      if done pos
      then pure (snd history, Win . opponent . color $ pos)
      else
      case bookPly book pos of
        Just r -> do
          pl <- evalRandIO r
          let history' = fmap (<> [pl]) history
          p2 <- case active of
            Player e1 Nothing -> do
              addPly e1 pl
              pure $ Player e1 Nothing
            Player e1 (Just (Searching bmc ic)) -> do
              stop e1
              atomically . readTChan $ bmc
              addPly e1 pl
              pure $ Player e1 Nothing
          p1 <- case passive of
            Player e2 Nothing -> do
              addPly e2 pl
              pure $ Player e2 Nothing
            Player e2 (Just (Pondering _ bmc ic)) -> do
              stop e2
              atomically . readTChan $ bmc
              replacePly e2 pl
              pure $ Player e2 Nothing
          clock' <- flipClock clock
          putStrLn $ "Book: " <> toSAN pos pl
          play (rt { history = history', active = p1, passive = p2, clock = clock' })
        Nothing -> do
          case active of
            Player e1 Nothing -> do
              let (Just wt, Just bt) = clockTimes clock
              (bmc, ic) <- search e1 [timeleft White wt, timeleft Black bt]
              play $ rt { active = Player e1 (Just (Searching bmc ic)) }
            Player e1 (Just (Searching bmc ic)) -> do
              sc <- newIORef Nothing
              itid <- liftIO . forkIO . forever $ do
                i <- atomically . readTChan $ ic
                case find isScore i of
                  Just (Score s _) -> writeIORef sc (Just s)
                  _ -> pure ()
              (bm, pndr) <- atomically . readTChan $ bmc
              killThread itid
              sc <- readIORef sc
              let history' = fmap (<> [bm]) history
              clock' <- flipClock clock
              addPly e1 bm
              p1 <- case passive of
                Player e2 Nothing -> do
                  addPly e2 bm
                  putStrLn $ "Move: " <> toSAN pos bm <> " (" <> show sc <> ")"
                  pure $ Player e2 Nothing
                Player e2 (Just (Pondering pndr bmc ic)) -> do
                  if bm == pndr
                  then do
                    ponderhit e2
                    putStrLn $ "Ponderhit: " <> toSAN pos bm <> " (" <> show sc <> ")"
                    pure $ Player e2 (Just (Searching bmc ic))
                  else do
                    stop e2
                    atomically . readTChan $ bmc
                    replacePly e2 bm
                    putStrLn $ "Pondermiss: " <> toSAN pos bm <> " (" <> show sc <> ")"
                    pure $ Player e2 Nothing
              p2 <- case pndr of
                Just pndr -> do
                  addPly e1 pndr
                  let (Just wt, Just bt) = clockTimes clock'
                  (bmc, ic) <- search e1 [ponder, timeleft White wt, timeleft Black bt]
                  pure $ Player e1 (Just (Pondering pndr bmc ic))
                Nothing -> 
                  pure $ Player e1 Nothing
              play $ rt { history = history', active = p1, passive = p2, clock = clock' }

toForest :: [Ply] -> Forest Ply
toForest [] = []
toForest (x:xs) = [Node x $ toForest xs]

isScore :: Info -> Bool
isScore Score{} = True
isScore _ = False
