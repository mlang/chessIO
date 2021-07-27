{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Game.Chess
import           Game.Chess.Polyglot
import           Test.HUnit

tests = test
  [ "t1" ~: "start position" ~: hashPosition startpos ~=? 0x463b96181691fc9c
  , "t2" ~: "after e4" ~: hashPosition "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" ~=? 0x823c9b50fd114196
  , "t3" ~: "after e4 d5" ~: hashPosition "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2" ~=? 0x0756b94461c50fb0
  , "t4" ~: "after e4 d5 e5" ~: hashPosition "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2" ~=? 0x662fafb965db29d4
  , "t5" ~: "after e4 d5 e5 f5" ~: hashPosition "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3" ~=? 0x22a48b5a8e47ff78
  , "t6" ~: "after e4 d5 e5 f5 Ke2" ~: hashPosition "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 0 3" ~=? 0x652a607ca3f242c1
  ]

main = runTestTTAndExit tests
