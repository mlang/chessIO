module Game.Chess.Tree where

import Data.Tree ( Tree(Node), Forest, foldTree )
import Game.Chess.Internal

positionTree :: Position -> Tree Position
positionTree pos = Node pos $ positionForest pos

positionForest :: Position -> Forest Position
positionForest pos = positionTree . unsafeDoPly pos <$> legalPlies pos

plyForest :: Position -> Forest Ply
plyForest pos = plyTree pos <$> legalPlies pos

plyTree :: Position -> Ply -> Tree Ply
plyTree pos ply = Node ply . plyForest $ unsafeDoPly pos ply

variationForest :: Position -> Forest [Ply]
variationForest = fmap pathTree . plyForest

pathTree :: Tree a -> Tree [a]
pathTree = foldTree $ \a xs -> Node [a] $ (fmap . fmap) (a :) xs
