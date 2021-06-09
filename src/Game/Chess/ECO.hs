{-# LANGUAGE TemplateHaskell #-}
module Game.Chess.ECO (
  -- * Data types
  ECO, Opening(..)
, defaultECO, scidECO
  -- * Query
, lookup
  -- * Conversion
, fromList, fromPGN, toList
  -- * Parsing scid .eco files
, readSCIDECOFile, scid
  -- * Template Haskell
, embedECO, eco_pgn, scid_eco
) where

import Prelude hiding (lookup)
import Game.Chess.Internal.ECO

defaultECO :: ECO
defaultECO = $$(embedECO eco_pgn "book/eco.pgn")
