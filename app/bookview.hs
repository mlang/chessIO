{-# LANGUAGE TemplateHaskell #-}
import Control.Monad (void)
import Data.List (elemIndex, intersperse)
import Data.List.Extra ( chunksOf, foldl' )
import Data.Maybe (fromJust, fromMaybe)
import Data.Tree (Tree(..))
import Data.Tree.Zipper
import qualified Data.Vector as Vec
import Game.Chess
import Game.Chess.Polyglot.Book
import Game.Chess.Tree
import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Focus as F
import qualified Brick.Widgets.List as L
import Brick.AttrMap (AttrMap, AttrName, attrMap)
import Brick.Util (on)
import Brick.Types (Widget)
import Brick.Widgets.Core ( showCursor, withAttr, hLimit, vLimit, hBox, vBox, str, strWrap , (<=>))

data Name = Board | List
          deriving (Show, Ord, Eq)

data St =
    St { _treepos :: TreePos Full [Ply]
       , _focusRing :: F.FocusRing Name
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = [ui] where
  ui = hBox [
       hLimit 9 $ list
     , hLimit 23 $ C.hCenter (B.border (board (position st) (Just . targetSquare $ st)))
               <=> B.border var
     ]
  var = strWrap . varToSAN startpos $ st ^. treepos & label
  list = L.renderList (drawPly (previousPosition st)) True (plyList st)
  drawPly p foc = putCursorIf foc (0,0) . str . toSAN p 
  putCursorIf True loc = showCursor List $ T.Location loc
  putCursorIf False _  = id

position, previousPosition :: St -> Position
position st = foldl' doPly startpos . label $ st ^. treepos
previousPosition st = foldl' doPly startpos . init . label $ st ^. treepos

targetSquare :: St -> Int
targetSquare st = st ^. treepos & label & Prelude.last & plyTarget

plyList :: St -> L.List Name Ply
plyList st = L.list List (Vec.fromList plies) 1
           & L.listSelectedL .~ n
 where
  ply = st ^. treepos & label & Prelude.last
  plies = st ^. treepos & forest & fmap (Prelude.last . rootLabel)
  n = ply `elemIndex` plies

board :: Position -> Maybe Int -> Widget Name
board pos tgt = vBox $ map ((hBox . spacer) . map pc) squares where
  squares = reverse $ chunksOf 8 [A1 .. H8]
  c sq | Just t <- tgt, t == toIndex sq = showCursor Board $ T.Location (0,0)
       | otherwise                      = id
  pc sq = c sq $ case pieceAt pos sq of
    Just (White, Pawn)   -> str "P"
    Just (White, Knight) -> str "N"
    Just (White, Bishop) -> str "B"
    Just (White, Rook)   -> str "R"
    Just (White, Queen)  -> str "Q"
    Just (White, King)   -> str "K"
    Just (Black, Pawn)   -> str "p"
    Just (Black, Knight) -> str "n"
    Just (Black, Bishop) -> str "b"
    Just (Black, Rook)   -> str "r"
    Just (Black, Queen)  -> str "q"
    Just (Black, King)   -> str "k"
    Nothing | isDark sq  -> str "+"
            | otherwise  -> str " "
  spacer = (str " " :) . (<> [str " "]) . intersperse (str " ")

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent e) = case e of
  V.EvKey V.KDown []        -> M.continue $ st & treepos %~ (fromMaybe <*> next)
  V.EvKey V.KUp []          -> M.continue $ st & treepos %~ (fromMaybe <*> prev)
  V.EvKey V.KRight []       -> M.continue $ st & treepos %~ (fromMaybe <*> firstChild)
  V.EvKey V.KLeft []        -> M.continue $ st & treepos %~ (fromMaybe <*> parent)
  V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
  V.EvKey V.KEsc []         -> M.halt st
  _                         -> M.continue st
appEvent st _ = M.continue st

theMap :: AttrMap
theMap = attrMap V.defAttr []

bookview :: M.App St e Name
bookview =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = pure
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const theMap
          , M.appChooseCursor = F.focusRingCursor (^.focusRing)
          }

initialState :: St
initialState = St tp fr where
  tp = fromJust . nextTree . fromForest . fmap pathTree . bookForest defaultBook
     $ startpos
  fr = F.focusRing [List, Board]

main :: IO ()
main = void $ M.defaultMain bookview initialState
