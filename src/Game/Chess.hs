{-|
Module      : Game.Chess
Description : Basic data types and functions related to the game of chess
Copyright   : (c) Mario Lang, 2020
License     : BSD3
Maintainer  : mlang@blind.guru
Stability   : experimental

Types for representing positions and plies and functions for move
generation and application.  Internally, quad bitboards are employed and plies
are stored as 16 bit values.  The move generation is fully
compliant to the standard rules of Chess.

This module does deliberately not implement
any search or evaluation functionality.  It is intended to be used
to lay the ground for communicating with other programs or players, hence the
package name chessIO.

The following modules implement more specific functionality:

- "Game.Chess.Polyglot": Polyglot opening book format
- "Game.Chess.SAN": Parse and print Standard Algebraic Notation.
- "Game.Chess.PGN": Parse and format Portable Game Notation files.
- "Game.Chess.Tree": Functions for converting a position to a tree.
- "Game.Chess.UCI": Control external engines using the Universal Chess Interface.
-}
module Game.Chess (
  -- * Chess positions
  Color(..), opponent
, IsSquare(toIndex, toRF), Sq(..), isLight, isDark
, PieceType(..), Castle(..)
, Position, startpos, color, moveNumber, halfMoveClock, pieceAt, inCheck
, castlingRights, canCastleKingside, canCastleQueenside
, insufficientMaterial, repetitions, enPassantSquare
  -- ** Converting from/to Forsyth-Edwards-Notation
, fromFEN, toFEN
  -- * Chess moves
, Ply(..), plySource, plyTarget, plyPromotion
  -- ** Convertion
, fromUCI, toUCI, fromPolyglot, toPolyglot
  -- ** Move generation
, legalPlies
  -- ** Executing moves
, doPly, unsafeDoPly
) where

import Game.Chess.Internal.Square
import Game.Chess.Internal
