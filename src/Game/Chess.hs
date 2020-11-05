{-|
Module      : Game.Chess
Description : Basic data types and functions related to the game of chess
Copyright   : (c) Mario Lang, 2020
License     : BSD3
Maintainer  : mlang@blind.guru
Stability   : experimental

A small collection of data types and functions to represent Chess positions
and moves including move generation and parsing from external sources.

This module does deliberately not implement
any search or evaluation functionality.  It is intended to be used
to lay the ground for communicating with other programs or players, hence the
package name chessIO.
-}
module Game.Chess (
  -- * Chess positions
  Color(..), opponent
, Sq(..), isLight, isDark
, PieceType(..), Castle(..)
, Position, startpos, color, moveNumber, halfMoveClock, pieceAt, inCheck
, castlingRights, canCastleKingside, canCastleQueenside
  -- ** Converting from/to Forsyth-Edwards-Notation
, fromFEN, toFEN
  -- ** Position tree
, positionTree, positionForest
  -- * Chess moves
, Ply(..)
  -- ** Converting from/to algebraic notation
, strictSAN, relaxedSAN, fromSAN, toSAN, unsafeToSAN, varToSAN, fromUCI, toUCI
, fromPolyglot, toPolyglot
  -- ** Move generation
, legalPlies
  -- ** Executing moves
, doPly, unsafeDoPly
  -- ** Move trees
, plyTree, plyForest
) where

import Game.Chess.Internal
import Game.Chess.SAN
import Game.Chess.Tree
