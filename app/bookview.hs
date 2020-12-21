{-# LANGUAGE TemplateHaskell #-}
import Control.Monad ( void )
import Data.List ( elemIndex, intersperse )
import Data.List.Extra ( chunksOf, foldl' )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Tree ( Tree(..) )
import Data.Tree.Zipper ( TreePos, Full
                        , label, forest, fromForest, nextTree
                        )
import qualified Data.Tree.Zipper as TreePos ( next, prev, firstChild, parent )
import qualified Data.Vector as Vec
import Game.Chess ( Color(..), PieceType(..), Sq(..), toIndex, isDark
                  , Position, startpos, pieceAt
                  , Ply, plyTarget, doPly, toSAN, varToSAN
                  )
import Game.Chess.Polyglot ( defaultBook, bookForest )
import Game.Chess.Tree ( pathTree )
import Lens.Micro ( over, (&), (^.), (.~) )
import Lens.Micro.TH ( makeLenses )
import qualified Graphics.Vty as V

import Brick.Main ( App(..), defaultMain, continue, halt )
import qualified Brick.Focus as F
import qualified Brick.Widgets.List as L
import Brick.AttrMap (AttrName, attrMap)
import Brick.Util (on)
import Brick.Types ( EventM, Next, Widget, Location(Location), BrickEvent( VtyEvent ) )
import Brick.Widgets.Core ( showCursor, withAttr, hLimit, vLimit, hBox, vBox, str, strWrap
                          , (<+>), (<=>)
                          )
import Brick.Widgets.Center ( hCenter )
import Brick.Widgets.Border ( border )

data Name = Board | List deriving (Show, Ord, Eq)

data St = St { _treePos :: TreePos Full [Ply]
             , _focusRing :: F.FocusRing Name
             }

makeLenses ''St

position, previousPosition :: St -> Position
position = foldl' doPly startpos . label . _treePos
previousPosition = foldl' doPly startpos . init . label . _treePos

targetSquare :: St -> Int
targetSquare = plyTarget . last . label . _treePos

plyList :: St -> L.List Name Ply
plyList st = L.list List (Vec.fromList plies) 1 & L.listSelectedL .~ n where
  ply = st ^. treePos & label & last
  plies = st ^. treePos & forest & fmap (last . rootLabel)
  n = ply `elemIndex` plies

selectedAttr :: AttrName
selectedAttr = "selected"

renderPosition :: Position -> Maybe Int -> Widget Name
renderPosition pos tgt = ranks <+> border board <=> files where
  ranks = vBox (str " " : map (str . show) [8, 7..1] <> [str " "])
  files = str "   a b c d e f g h  "
  board = hLimit 17 . vLimit 8 . vBox $ map (hBox . spacer . map pc) squares
  squares = reverse $ chunksOf 8 [A1 .. H8]
  c sq | Just t <- tgt, t == toIndex sq = showCursor Board $ Location (0,0)
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

next, prev, firstChild, parent, nextCursor :: St -> EventM Name (Next St)
next = continue . over treePos (fromMaybe <*> TreePos.next)
prev = continue . over treePos (fromMaybe <*> TreePos.prev)
firstChild = continue . over treePos (fromMaybe <*> TreePos.firstChild)
parent = continue . over treePos (fromMaybe <*> TreePos.parent)
nextCursor = continue . over focusRing F.focusNext

app :: App St e Name
app = App { .. } where
  appStartEvent = pure
  appDraw st = [ui] where
    ui = hBox [hLimit 9 list, hLimit 23 game]
    list = L.renderList (drawPly (previousPosition st)) True (plyList st)
    drawPly p foc = putCursorIf foc (0,0)
                  . withAttrIf foc selectedAttr
                  . str . toSAN p 
    putCursorIf True loc = showCursor List $ Location loc
    putCursorIf False _  = id
    withAttrIf True attr = withAttr attr
    withAttrIf False _   = id
    game = hCenter board <=> border var
    board = renderPosition (position st) (Just . targetSquare $ st)
    var = strWrap . varToSAN startpos $ st ^. treePos & label
  appHandleEvent st (VtyEvent e) = case e of
    V.EvKey V.KDown []        -> next st
    V.EvKey (V.KChar 'j') []  -> next st
    V.EvKey V.KUp []          -> prev st
    V.EvKey (V.KChar 'k') []  -> prev st
    V.EvKey V.KRight []       -> firstChild st
    V.EvKey (V.KChar 'l') []  -> firstChild st
    V.EvKey V.KLeft []        -> parent st
    V.EvKey (V.KChar 'h') []  -> parent st
    V.EvKey (V.KChar '\t') [] -> nextCursor st
    V.EvKey V.KEsc []         -> halt st
    _                         -> continue st
  appHandleEvent st _          = continue st
  appAttrMap = const $ attrMap V.defAttr
             [(selectedAttr, V.white `on` V.green)
             ]
  appChooseCursor = F.focusRingCursor _focusRing

initialState :: St
initialState = St tp fr where
  tp = fromJust . nextTree . fromForest . fmap pathTree . bookForest defaultBook
     $ startpos
  fr = F.focusRing [List, Board]

main :: IO ()
main = void $ defaultMain app initialState
