module Main where

import Control.Parallel.Strategies
import Data.Foldable
import Data.Maybe
import Data.Time.Clock
import Data.Traversable
import Game.Chess
import GHC.Generics (Generic)
import System.Directory
import System.Exit
import System.IO

main :: IO ()
main = do
  start <- getCurrentTime
  exists <- doesFileExist "perftsuite.epd"
  ok <- if exists
    then do
      suite <- readTestSuite "perftsuite.epd"
      runTestSuite suite
    else do
      for_ [0..6] $ \n -> do
        putStrLn $ showResult n (perft n startpos)
        hFlush stdout
      pure False
  end <- getCurrentTime
  putStrLn $ "Time: " <> show (diffUTCTime end start)
  exitWith $ if ok then ExitSuccess else ExitFailure 1

data PerftResult = PerftResult { nodes :: !Integer } deriving (Eq, Generic, Show)
instance NFData PerftResult

instance Semigroup PerftResult where
  PerftResult n1 <> PerftResult n2 = PerftResult $ n1 + n2

instance Monoid PerftResult where
  mempty = PerftResult 0

showResult :: Int -> PerftResult -> String
showResult depth PerftResult{nodes} = show depth <> " " <> show nodes

perft :: Int -> Position -> PerftResult
perft 0 _ = PerftResult 1
perft 1 p = PerftResult . fromIntegral . length $
            applyMove p <$> moves p
perft 2 p = fold . map (perft 1) $ applyMove p <$> moves p
perft n p = fold . withStrategy (parList rdeepseq) . map (perft $ pred n) $
            applyMove p <$> moves p

runTestSuite :: [(Position, [(Int, PerftResult)])] -> IO Bool
runTestSuite = fmap (all id) . traverse (uncurry test) where
  test pos ((depth, expected) : more)
    | result == expected
    = do
      putStrLn $ "OK   " <> fen <> " ;D" <> show depth <> " "
              <> show (nodes expected)
      hFlush stdout
      test pos more
    | otherwise
    = do
      putStrLn $ "FAIL " <> fen <> " ;D" <> show depth <> " "
              <> show (nodes expected) <> " /= " <> show (nodes result)
      pure False
   where result = perft depth pos
         fen = toFEN pos
  test _ [] = pure True

readTestSuite :: FilePath -> IO [(Position, [(Int, PerftResult)])]
readTestSuite fp = do
  epd <- readFile fp
  pure $ fmap readData . (\ws -> (fromJust (fromFEN (unwords $ take 6 ws)), drop 6 ws)) . words <$> lines epd
 where
  readData [] = []
  readData ((';':'D':d):v:xs) = (read d, PerftResult $ read v) : readData xs
  readData _ = error "Failed to parse test suite"
