{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
import           Brick.AttrMap        (AttrName, attrMap)
import qualified Brick.Focus          as F
import           Brick.Main           (App (..), continue, defaultMain, halt)
import           Brick.Types          (BrickEvent (VtyEvent), EventM,
                                       Location (Location), Next, Widget)
import           Brick.Util           (on)
import           Brick.Widgets.Border (border, borderWithLabel)
import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Core   (hBox, hLimit, showCursor, str, strWrap,
                                       txt, txtWrap, vBox, vLimit, withAttr,
                                       (<+>), (<=>))
import qualified Brick.Widgets.List   as L
import           Control.Lens         (makeLenses, over, view, (&), (.~), (^.))
import           Control.Monad        (void)
import           Data.Foldable        (foldl', toList)
import           Data.Ix
import           Data.List            (elemIndex, intersperse)
import           Data.List.Extra      (chunksOf)
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.Tree            (Tree (..), foldTree)
import           Data.Tree.Zipper     (Full, TreePos, fromForest, label,
                                       nextTree)
import qualified Data.Tree.Zipper     as TreePos
import qualified Data.Vector          as Vector
import           Game.Chess           (Color (..), PieceType (..), Ply,
                                       Position, Square (A1, H8), color, doPly,
                                       isDark, pieceAt, plyTarget, startpos,
                                       toFEN)
import           Game.Chess.ECO       (Opening (..), defaultECO)
import qualified Game.Chess.ECO       as ECO
import           Game.Chess.PGN       (pgnForest, readPGNFile)
import           Game.Chess.Polyglot  (bookForest, defaultBook,
                                       readPolyglotFile)
import           Game.Chess.SAN       (toSAN, varToSAN)
import           Game.Chess.Tree      (plyForest)
import qualified Graphics.Vty         as V
import           System.Environment   (getArgs)
import           System.FilePath

data Name = List | Board | BoardStyle deriving (Show, Ord, Eq)

type Style a = Position -> Square -> Widget a

data St = St { _initialPosition :: Position
             , _treePos         :: TreePos Full (NonEmpty Ply)
             , _boardStyle      :: L.List Name (String, Style Name)
             , _focusRing       :: F.FocusRing Name
             }

makeLenses ''St

initialState :: St
initialState = St { .. } where
  _initialPosition = startpos
  _treePos = fromJust . nextTree . fromForest
           $ pathTree <$> bookForest defaultBook _initialPosition
  _boardStyle = L.list BoardStyle (Vector.fromList styles) 1
  _focusRing = F.focusRing [List, Board, BoardStyle]

position, previousPosition :: St -> Position
position st = foldl' doPly (st^.initialPosition) (st^.treePos & label)
previousPosition st = foldl' doPly (st^.initialPosition) (st^.treePos & label & NonEmpty.init)

targetSquare :: St -> Square
targetSquare = plyTarget . NonEmpty.last . label . view treePos

elemList :: Eq a => n -> a -> [a] -> L.List n a
elemList n x xs = L.list n (Vector.fromList xs) 1 & L.listSelectedL .~ i where
  i = x `elemIndex` xs

plyList :: St -> L.List Name Ply
plyList (_treePos -> tp) = elemList List ply plies where
  ply = NonEmpty.last . TreePos.label $ tp
  plies = fmap (NonEmpty.last . rootLabel) . TreePos.forest $ tp

selectedAttr :: AttrName
selectedAttr = "selected"

renderPosition :: Position -> Color -> Maybe Square -> Style Name -> Widget Name
renderPosition pos persp tgt sty = ranks <+> border board <=> files where
  rev :: [a] -> [a]
  rev = if persp == Black then reverse else id
  ranks = vBox (str " " : map (str . show) (rev [8 :: Int, 7..1]) <> [str " "])
  files = str $ rev "   a b c d e f g h   "
  board = hLimit 17 . vLimit 8 . vBox $ map (hBox . spacer . map pc) squares
  squares = reverse $ chunksOf 8 $ rev [A1 .. H8]
  pc sq = putCursorIf (tgt == Just sq) Board (0,0) $ sty pos sq
  spacer = (str " " :) . (<> [str " "]) . intersperse (str " ")

allPieces :: ((Color, PieceType), (Color, PieceType))
allPieces = ((Black, Pawn), (White, King))

english :: Style a
english pos sq = case pieceAt pos sq of
  Just piece           -> str . pure $ "pnbrqkPNBRQK" !! index allPieces piece
  Nothing | isDark sq  -> str "+"
          | otherwise  -> str " "

styles :: [(String, Style a)]
styles = [ ("English",  english)
         , ("Deutsch",  german)
         , ("Figurine", figurine)
         ]
 where
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
withAttrIf True attr = withAttr attr
withAttrIf False _   = id

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

keyMap :: Map V.Event Command
keyMap = Map.fromList $ cursor <> vi <> common where
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

cbookview :: App St e Name
cbookview = App { .. } where
  appStartEvent = pure
  appDraw st = [ui] where
    ui = hBox [ hLimit 9 list
              , hLimit 23 $ hCenter board <=> str " " <=> eco
              , hCenter . hLimit 40 $ str " " <=> var
              ]
      <=> str " "
      <=> (str "FEN: " <+> fen)
      <=> str " "
      <=> hBox [str "Board style (+/- to change): ", style]
      <=> hBox [str "Up/Down (kj) = change ply, Left/Right (hl) = back/forward"
               , hCenter $ str " "
               , str "ESC (q) = Quit"
               ]
    eco = maybe (str " ") drawECO (ECO.lookup (position st) defaultECO)
    drawECO co = borderWithLabel (str "ECO " <+> txt (coCode co)) $
      case coVariation co of
        Nothing        -> txtWrap (coName co)
        Just variation -> txtWrap (coName co) <=> txtWrap variation
    style = vLimit 1 $ L.renderList drawStyle True (st^.boardStyle)
    drawStyle foc (n, _) = putCursorIf foc BoardStyle (0,0) $ str n
    selectedStyle = maybe english (snd . snd) $
      st^.boardStyle & L.listSelectedElement
    list = L.renderList (drawPly (previousPosition st)) True (plyList st)
    drawPly p foc = putCursorIf foc List (0,0)
                  . withAttrIf foc selectedAttr
                  . str . toSAN p
    board = renderPosition (position st) (color (previousPosition st)) (Just . targetSquare $ st) selectedStyle
    var = strWrap . varToSAN (st^.initialPosition) $ st^.treePos & TreePos.label & toList
    fen = str . toFEN $ position st
  appHandleEvent st (VtyEvent e) = fromMaybe continue (Map.lookup e keyMap) st
  appHandleEvent st _            = continue st
  appAttrMap = const $ attrMap V.defAttr
             [(selectedAttr, V.white `on` V.green)
             ]
  appChooseCursor = F.focusRingCursor (view focusRing)

loadForest :: (Position -> [Tree Ply]) -> Position -> St -> Maybe St
loadForest f p st = case f p of
  [] -> Nothing
  ts -> Just $ st & initialPosition .~ p & treePos .~ tp where
    tp = fromJust . nextTree . fromForest $ pathTree <$> ts

pathTree :: Tree a -> Tree (NonEmpty a)
pathTree = foldTree $ \a -> Node (pure a) . (fmap . fmap) (NonEmpty.cons a)

main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> void $ defaultMain cbookview initialState
    [fp] -> case takeExtension fp of
      ".bin" -> do
        book <- readPolyglotFile fp
        case loadForest (bookForest book) startpos initialState of
          Just st -> void $ defaultMain cbookview st
          Nothing -> putStrLn "No moves found in book"
      ".pgn" -> readPGNFile fp >>= \case
        Right pgn -> case loadForest (const $ pgnForest pgn) startpos initialState of
          Just st -> void $ defaultMain cbookview st
          Nothing -> putStrLn "No moves found in PGN"
        Left err -> putStrLn err
      ext -> putStrLn $ "Unknown extension " <> ext <> ", only .bin (polyglot) and .pgn is supposed"
    _ -> putStrLn "Too many arguments."
