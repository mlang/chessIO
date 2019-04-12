module Game.Chess.PGN.Export where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Tree
import Game.Chess
import Game.Chess.PGN.Types

type RAVOrder a = (Forest PlyData -> a) -> Forest PlyData -> [a]

breadthFirst, depthFirst :: RAVOrder a
breadthFirst _ [] = []
breadthFirst f ts = pure $ f ts
depthFirst f = fmap $ f . pure

pgnDoc :: RAVOrder (Doc ann) -> PGN -> Doc ann
pgnDoc ro = vsep . fmap (gameDoc ro)

gameDoc :: RAVOrder (Doc ann) -> ([(ByteString, String)], (Outcome, Forest PlyData)) -> Doc ann
gameDoc ro (tl, mt)
  | null tl = moveDoc ro pos mt
  | otherwise = tagsDoc tl <> line <> line <> moveDoc ro pos mt
 where
  pos | Just fen <- lookup "FEN" tl = fromJust $ fromFEN fen
      | otherwise = startpos

tagsDoc :: [(ByteString, String)] -> Doc ann
tagsDoc = vsep . fmap tagpair where
  tagpair (k, v) = brackets $ pretty (BS.unpack k) <+> dquotes (pretty v)

moveDoc :: RAVOrder (Doc ann) -> Position -> (Outcome, Forest PlyData) -> Doc ann
moveDoc ro pos (o,ts) = fillSep (go pos True ts <> [outcome o]) <> line where
  go _ _ [] = []
  go pos pmn (t:ts)
    | color pos == White || pmn
    = mn:snag <> (san:pnag) <> rav <> go pos' (not . null $ rav) (subForest t)
    | otherwise
    = snag <> (san:pnag) <> rav <> go pos' (not . null $ rav) (subForest t)
   where
    pl = ply . rootLabel $ t
    san = pretty (toSAN pos pl)
    pos' = unsafeApplyMove pos pl
    pnag = nag <$> prefixNAG (rootLabel t)
    mn = pretty (moveNumber pos) <> if color pos == White then "." else "..."
    rav = ro (parens . fillSep . go pos True) ts
    snag = nag <$> suffixNAG (rootLabel t)
  outcome (Win White) = "1-0"
  outcome (Win Black) = "0-1"
  outcome Draw        = "1/2-1/2"
  outcome Undecided   = "*"
  nag n = "$" <> pretty n

