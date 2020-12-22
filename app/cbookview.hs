{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding (last)
import Control.Monad ( void )
import Data.Foldable ( foldl', toList )
import Data.List ( elemIndex, intersperse )
import Data.List.Extra ( chunksOf )
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe ( fromJust, fromMaybe )
import Data.Tree ( Tree(..), Forest )
import Data.Tree.Zipper ( TreePos, Full
                        , label, forest, fromForest, nextTree, prevTree
                        )
import qualified Data.Tree.Zipper as TreePos
import qualified Data.Vector as Vec
import Game.Chess ( Color(..), PieceType(..), Sq(..), toIndex, isDark
                  , Position, color, startpos, pieceAt
                  , Ply, plyTarget, doPly, toSAN, varToSAN
                  )
import Game.Chess.Polyglot ( defaultBook, bookForest, readPolyglotFile )
import Game.Chess.PGN
import Game.Chess.Tree ( plyForest, pathTree )
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
import System.FilePath
import System.Environment ( getArgs )

data Name = List | Board deriving (Show, Ord, Eq)

data St = St { _initialPosition :: Position
             , _treePos :: TreePos Full (NonEmpty Ply)
             , _focusRing :: F.FocusRing Name
             }

makeLenses ''St

position, previousPosition :: St -> Position
position st = foldl' doPly (st^.initialPosition) (st^.treePos & label)
previousPosition st = foldl' doPly (st^.initialPosition) (st^.treePos & label & NonEmpty.init)

targetSquare :: St -> Int
targetSquare = plyTarget . NonEmpty.last . label . _treePos

elemList :: Eq a => n -> a -> [a] -> L.List n a
elemList n x xs = L.list n (Vec.fromList xs) 1 & L.listSelectedL .~ i where
  i = x `elemIndex` xs

plyList :: St -> L.List Name Ply
plyList (_treePos -> tp) = elemList List ply plies where
  ply = NonEmpty.last . label $ tp
  plies = fmap (NonEmpty.last . rootLabel) . forest $ tp

selectedAttr :: AttrName
selectedAttr = "selected"

renderPosition :: Position -> Color -> Maybe Int -> Widget Name
renderPosition pos persp tgt = ranks <+> border board <=> files where
  rev :: [a] -> [a]
  rev = if persp == Black then reverse else id
  ranks = vBox (str " " : map (str . show) (rev [8, 7..1]) <> [str " "])
  files = str $ rev "   a b c d e f g h   "
  board = hLimit 17 . vLimit 8 . vBox $ map (hBox . spacer . map pc) squares
  squares = reverse $ chunksOf 8 $ rev [A1 .. H8]
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

next, prev, firstChild, parent, root, nextCursor :: St -> EventM Name (Next St)
next = continue . over treePos (fromMaybe <*> TreePos.next)
prev = continue . over treePos (fromMaybe <*> TreePos.prev)
firstChild = continue . over treePos (fromMaybe <*> TreePos.firstChild)
parent = continue . over treePos (fromMaybe <*> TreePos.parent)
root = continue . over treePos TreePos.root
nextCursor = continue . over focusRing F.focusNext

allPlies, internalBook :: St -> EventM Name (Next St)
allPlies = continue . (fromMaybe <*> loadForest plyForest startpos)
internalBook = continue . (fromMaybe <*> loadForest (bookForest defaultBook) startpos)

app :: App St e Name
app = App { .. } where
  appStartEvent = pure
  appDraw st = [ui] where
    ui = hBox [ hLimit 9 list
              , hLimit 23 $ hCenter board
              , hCenter . hLimit 40 $ str " " <=> var
              ]
      <=> hBox [str "Up/Down (kj) = change ply, Left/Right (hl) = back/forward"
               , hCenter $ str " "
               , str "ESC = Quit"
               ]
    list = L.renderList (drawPly (previousPosition st)) True (plyList st)
    drawPly p foc = putCursorIf foc (0,0)
                  . withAttrIf foc selectedAttr
                  . str . toSAN p 
    putCursorIf True loc = showCursor List $ Location loc
    putCursorIf False _  = id
    withAttrIf True attr = withAttr attr
    withAttrIf False _   = id
    board = renderPosition (position st) (color (previousPosition st)) (Just . targetSquare $ st)
    var = strWrap . varToSAN (st^.initialPosition) $ st^.treePos & label & toList
  appHandleEvent st (VtyEvent e) = case e of
    V.EvKey V.KDown []        -> next st
    V.EvKey (V.KChar 'j') []  -> next st
    V.EvKey V.KUp []          -> prev st
    V.EvKey (V.KChar 'k') []  -> prev st
    V.EvKey V.KRight []       -> firstChild st
    V.EvKey (V.KChar 'l') []  -> firstChild st
    V.EvKey V.KLeft []        -> parent st
    V.EvKey (V.KChar 'h') []  -> parent st
    V.EvKey V.KHome []        -> root st
    V.EvKey (V.KChar '\t') [] -> nextCursor st
    V.EvKey (V.KChar 'a') []  -> allPlies st
    V.EvKey (V.KChar 'd') []  -> internalBook st
    V.EvKey V.KEsc []         -> halt st
    _                         -> continue st
  appHandleEvent st _          = continue st
  appAttrMap = const $ attrMap V.defAttr
             [(selectedAttr, V.white `on` V.green)
             ]
  appChooseCursor = F.focusRingCursor _focusRing

loadForest :: (Position -> Forest Ply) -> Position -> St -> Maybe St
loadForest f p st = case f p of
  [] -> Nothing
  ts -> Just $ st & initialPosition .~ p & treePos .~ tp where
    tp = fromJust . nextTree . fromForest . fmap pathTree $ ts

initialState :: St
initialState = St pos tp fr where
  tp = fromJust . nextTree . fromForest . fmap pathTree . bookForest defaultBook
     $ pos
  pos = startpos
  fr = F.focusRing [List, Board]

main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> void $ defaultMain app initialState
    [fp] -> case takeExtension fp of
      ".bin" -> do
        book <- readPolyglotFile fp
        case loadForest (bookForest book) startpos initialState of
          Just st -> void $ defaultMain app st
          Nothing -> putStrLn "No moves found in book"
      ".pgn" -> readPGNFile fp >>= \case
        Right pgn -> case loadForest (const $ pgnForest pgn) startpos initialState of
          Just st -> void $ defaultMain app st
          Nothing -> putStrLn "No moves found in PGN"
        Left err -> putStrLn err
      ext -> putStrLn $ "Unknown extension " <> ext <> ", only .bin (polyglot) and .pgn is supposed"
    _ -> putStrLn "Too many arguments."
