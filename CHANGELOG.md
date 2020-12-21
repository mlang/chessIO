# Releases

## chessIO (unreleased)

- Add strict apply to internal `foldBits` function which improves performance
  and memory footprint.
- Avoid cycles in `bookForest`.
- Don't reexport tree related functions from Game.Chess.
- Move all internal modules into Internal/.
- Rename `Game.Chess.Polyglot.Book` to just `Game.Chess.Polyglot` and don't
  expose `Game.Chess.Polyglot.Hash`.
- New tool `bookview`: terminal chess book opening explorer.

## chessIO 0.5.0.0

- Split SAN parsing code into a separate module.
- Adapt to VisualStream change in Megaparsec >= 9.
- Use Maybe to indicate that bestmove in UCI can be empty.
- instance Storable QuadBitboard

## chessIO 0.4.0.0

- Support for letting UCI engines ponder.
- Avoid a branch to further speed up move generation.

