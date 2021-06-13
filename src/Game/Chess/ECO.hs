{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Game.Chess.ECO
Description : Encyclopedia of Chess Openings
Copyright   : (c) Mario Lang, 2021
License     : BSD3
Maintainer  : mlang@blind.guru
Stability   : experimental

Types and functions to name chess positions according to the opening used.

Two commonly used file formats for opening classification are supported.
-}
module Game.Chess.ECO (
  -- * Data types
  ECO, Opening(..)
, defaultECO
  -- * Query
, lookup
  -- * Conversion
, fromList, fromPGN, toList
  -- * Parsing scid .eco files
, readSCIDECOFile, scid
  -- * Template Haskell
, embedECO, eco_pgn, scid_eco
) where

import           Game.Chess.Internal.ECO
import           Prelude                 hiding (lookup)

defaultECO :: ECO
defaultECO = $$(embedECO eco_pgn "book/eco.pgn")
