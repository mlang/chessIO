{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding (last)
import Control.Monad ( void )
import Data.Foldable ( foldl', toList )
import Data.Ix
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
                  , Position, color, startpos, pieceAt, toFEN
                  , Ply, plyTarget, doPly
                  )
import Game.Chess.Polyglot ( defaultBook, bookForest, readPolyglotFile )
import Game.Chess.PGN ( readPGNFile, pgnForest )
import Game.Chess.SAN ( toSAN, varToSAN )
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

data Name = List | Board | BoardStyle deriving (Show, Ord, Eq)

type Style a = Position -> Sq -> Widget a

data St = St { _initialPosition :: Position
             , _treePos :: TreePos Full (NonEmpty Ply)
             , _boardStyle :: L.List Name (String, Style Name)
             , _focusRing :: F.FocusRing Name
             }

makeLenses ''St

position, previousPosition :: St -> Position
position st = foldl' doPly (st^.initialPosition) (st^.treePos & label)
previousPosition st = foldl' doPly (st^.initialPosition) (st^.treePos & label & NonEmpty.init)

targetSquare :: St -> Int
targetSquare = plyTarget . NonEmpty.last . label . (^. treePos)

elemList :: Eq a => n -> a -> [a] -> L.List n a
elemList n x xs = L.list n (Vec.fromList xs) 1 & L.listSelectedL .~ i where
  i = x `elemIndex` xs

plyList :: St -> L.List Name Ply
plyList (_treePos -> tp) = elemList List ply plies where
  ply = NonEmpty.last . label $ tp
  plies = fmap (NonEmpty.last . rootLabel) . forest $ tp

selectedAttr :: AttrName
selectedAttr = "selected"

renderPosition :: Position -> Color -> Maybe Int -> Style Name -> Widget Name
renderPosition pos persp tgt sty = ranks <+> border board <=> files where
  rev :: [a] -> [a]
  rev = if persp == Black then reverse else id
  ranks = vBox (str " " : map (str . show) (rev [8 :: Int, 7..1]) <> [str " "])
  files = str $ rev "   a b c d e f g h   "
  board = hLimit 17 . vLimit 8 . vBox $ map (hBox . spacer . map pc) squares
  squares = reverse $ chunksOf 8 $ rev [A1 .. H8]
  c sq | Just t <- tgt, t == toIndex sq = showCursor Board $ Location (0,0)
       | otherwise                      = id
  pc sq = c sq $ sty pos sq
  spacer = (str " " :) . (<> [str " "]) . intersperse (str " ")

allPieces :: ((Color, PieceType), (Color, PieceType))
allPieces = ((Black, Pawn), (White, King))

english :: Style a
english pos sq = case pieceAt pos sq of
  Just piece           -> str . pure $ "pnbrqkPNBRQK" !! index allPieces piece
  Nothing | isDark sq  -> str "+"
          | otherwise  -> str " "

styles :: [(String, Style a)]
styles = map where
  map = [ ("English",  english)
        , ("Deutsch",  german)
        , ("Figurine", figurine)
        ]
  german pos sq = case pieceAt pos sq of
    Just piece           -> str . pure $ "bsltdkBSLTDK" !! index allPieces piece
    Nothing | isDark sq  -> str "+"
            | otherwise  -> str " "
  figurine pos sq = case pieceAt pos sq of
    Just piece           -> str . pure $ "♟♞♝♜♛♚♙♘♗♖♕♔" !! index allPieces piece
    Nothing | isDark sq  -> str "+"
            | otherwise  -> str " "

putCursorIf :: Bool -> n -> (Int, Int) -> Widget n -> Widget n
putCursorIf True n loc = showCursor n $ Location loc
putCursorIf False _ _  = id

withAttrIf :: Bool -> AttrName -> Widget n -> Widget n
withAttrIf True attr   = withAttr attr
withAttrIf False _     = id

type Command = St -> EventM Name (Next St)

next, prev, firstChild, parent, root, firstLeaf :: Command
next       = continue . over treePos (fromMaybe <*> TreePos.next)
prev       = continue . over treePos (fromMaybe <*> TreePos.prev)
firstChild = continue . over treePos (fromMaybe <*> TreePos.firstChild)
parent     = continue . over treePos (fromMaybe <*> TreePos.parent)
root       = continue . over treePos TreePos.root
firstLeaf  = continue . over treePos go where
  go tp = maybe tp go $ TreePos.firstChild tp

nextCursor, prevCursor :: Command
nextCursor = continue . over focusRing F.focusNext
prevCursor = continue . over focusRing F.focusPrev

allPlies, internalBook :: Command
allPlies     = continue . (fromMaybe <*> loadForest plyForest startpos)
internalBook = continue . (fromMaybe <*> loadForest (bookForest defaultBook) startpos)

nextStyle, prevStyle :: Command
nextStyle = continue . over boardStyle L.listMoveDown
prevStyle = continue . over boardStyle L.listMoveUp

keyMap :: [(V.Event, Command)]
keyMap = cursor <> vi <> common where
  cursor =
    [ (V.EvKey V.KDown [],       next)
    , (V.EvKey V.KUp [],         prev)
    , (V.EvKey V.KRight [],      firstChild)
    , (V.EvKey V.KLeft [],       parent)
    , (V.EvKey V.KHome [],       root)
    , (V.EvKey V.KEnd [],        firstLeaf)
    ]
  common =
    [ (V.EvKey (V.KChar '\t') [],        nextCursor)
    , (V.EvKey (V.KChar '\t') [V.MMeta], prevCursor)
    , (V.EvKey (V.KChar 'a') [],         allPlies)
    , (V.EvKey (V.KChar 'd') [],         internalBook)
    , (V.EvKey (V.KChar '+') [],         nextStyle)
    , (V.EvKey (V.KChar '-') [],         prevStyle)
    , (V.EvKey V.KEsc [],                halt)
    , (V.EvKey (V.KChar 'q') [],         halt)
    ]
  vi =
    [ (V.EvKey (V.KChar 'j') [], next)
    , (V.EvKey (V.KChar 'k') [], prev)
    , (V.EvKey (V.KChar 'l') [], firstChild)
    , (V.EvKey (V.KChar 'h') [], parent)
    ]

app :: App St e Name
app = App { .. } where
  appStartEvent = pure
  appDraw st = [ui] where
    ui = hBox [ hLimit 9 list
              , hLimit 23 $ hCenter board
              , hCenter . hLimit 40 $ str " " <=> var
              ]
      <=> (str "FEN: " <+> fen)
      <=> str " "
      <=> hBox [str "Board style (+/- to change): ", style]
      <=> hBox [str "Up/Down (kj) = change ply, Left/Right (hl) = back/forward"
               , hCenter $ str " "
               , str "ESC (q) = Quit"
               ]
    style = vLimit 1 $ L.renderList drawStyle True (st^.boardStyle)
    drawStyle foc (n, _) = putCursorIf foc BoardStyle (0,0) $ str n
    selectedStyle = maybe english (snd . snd) $
      st^.boardStyle & L.listSelectedElement
    list = L.renderList (drawPly (previousPosition st)) True (plyList st)
    drawPly p foc = putCursorIf foc List (0,0)
                  . withAttrIf foc selectedAttr
                  . str . toSAN p 
    board = renderPosition (position st) (color (previousPosition st)) (Just . targetSquare $ st) selectedStyle
    var = strWrap . varToSAN (st^.initialPosition) $ st^.treePos & label & toList
    fen = str . toFEN $ position st
  appHandleEvent st (VtyEvent e) = fromMaybe continue (lookup e keyMap) st
  appHandleEvent st _            = continue st
  appAttrMap = const $ attrMap V.defAttr
             [(selectedAttr, V.white `on` V.green)
             ]
  appChooseCursor = F.focusRingCursor (^. focusRing)

loadForest :: (Position -> Forest Ply) -> Position -> St -> Maybe St
loadForest f p st = case f p of
  [] -> Nothing
  ts -> Just $ st & initialPosition .~ p & treePos .~ tp where
    tp = fromJust . nextTree . fromForest . fmap pathTree $ ts

initialState :: St
initialState = St pos tp sl fr where
  tp = fromJust . nextTree . fromForest . fmap pathTree . bookForest defaultBook
     $ pos
  pos = startpos
  fr = F.focusRing [List, Board, BoardStyle]
  sl = L.list BoardStyle (Vec.fromList styles) 1

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
