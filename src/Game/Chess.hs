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

- "Game.Chess.ECO": Parse the Encyclopedia of Chess Openings and lookup positions
- "Game.Chess.Polyglot": Polyglot opening book format
- "Game.Chess.SAN": Parse and print Standard Algebraic Notation.
- "Game.Chess.PGN": Parse and format Portable Game Notation files.
- "Game.Chess.Tree": Functions for converting a position to a tree.
- "Game.Chess.UCI": Control external engines using the Universal Chess Interface.
-}
module Game.Chess (
  -- * Chess positions
  Color(..), opponent
, Square(A1, A2, A3, A4, A5, A6, A7, A8,
         B1, B2, B3, B4, B5, B6, B7, B8,
         C1, C2, C3, C4, C5, C6, C7, C8,
         D1, D2, D3, D4, D5, D6, D7, D8,
         E1, E2, E3, E4, E5, E6, E7, E8,
         F1, F2, F3, F4, F5, F6, F7, F8,
         G1, G2, G3, G4, G5, G6, G7, G8,
         H1, H2, H3, H4, H5, H6, H7, H8)
, rank, Rank(Rank1, Rank2, Rank3, Rank4, Rank5, Rank6, Rank7, Rank8)
, file, File(FileA, FileB, FileC, FileD, FileE, FileF, FileG, FileH)
, rankFile, isLight, isDark
, rankChar, fileChar, toCoord
, PieceType(..), Castle(..)
, Position, startpos, color, moveNumber, halfMoveClock, pieceAt, inCheck
, castlingRights, canCastleKingside, canCastleQueenside
, insufficientMaterial, repetitions, enPassantSquare
  -- ** Converting from/to Forsyth-Edwards-Notation
, fromFEN, toFEN
  -- * Chess moves
, Ply, plySource, plyTarget, plyPromotion
  -- ** Convertion
, fromUCI, toUCI, fromPolyglot, toPolyglot
  -- ** Move generation
, legalPlies
  -- ** Executing moves
, doPly, unsafeDoPly
) where

import Game.Chess.Internal.Square
import Game.Chess.Internal
