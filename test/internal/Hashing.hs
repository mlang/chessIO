module Main where

import           Numeric                     (showHex)
import           Data.Maybe                  (fromJust)
import           Game.Chess.Internal         (Position (Position, key), Ply, doPly, startpos, legalPlies, fromFEN, toFEN, unsafeDoPly, hashPosition, repetitionInfo, move)
import           Game.Chess.Internal.Square
import           System.Exit                 (exitSuccess, exitFailure)
import           Test.QuickCheck             (Arbitrary, arbitrary, Gen, chooseInt, sized, quickCheckResult, withMaxSuccess,
                                              Result (Success))

newtype MoveSequence = MoveSequence {unwrapMoveSequence :: [Ply]} deriving (Show, Eq, Ord)

instance Arbitrary MoveSequence where
     arbitrary = fmap MoveSequence . sized $ loop startpos
      where
        loop :: Position -> Int -> Gen [Ply]
        loop _ 0 = return []
        loop pos n = do
            let moves = legalPlies pos
            case length moves of
                0 -> return []
                l -> do
                    ix <- chooseInt (0, length moves - 1)
                    let move = moves !! ix
                    subresult <- loop (doPly pos move) $ n - 1
                    return $ move:subresult

toPositionSequence :: [Ply] -> [Position]
toPositionSequence plies =
  let inner pos []     = [pos]
      inner pos (p:ps) = pos:inner (unsafeDoPly pos p) ps
    in inner startpos plies

-- Generate a random sequence of moves from the start position. For each ply, convert the board to FEN and back
-- and check that the newly-computed hashes match the incrementally-updated ones.
testGameTreeHashes :: MoveSequence -> Bool
testGameTreeHashes = all hashesMatch . toPositionSequence . unwrapMoveSequence
  where
    hashesMatch pos = let
        k = key pos
        h = hashPosition pos
      in k == h

main = do
    result <- quickCheckResult testGameTreeHashes
    case result of
        Success{} -> exitSuccess
        _ -> exitFailure