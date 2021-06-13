# Releases

## chessIO 0.7.0.0

- Add HasCallStack to partial doPly and toSAN (thanks Oleg Grenrus)
- instance IsString Position based on fromFEN
- instance Hashable Position
- instance Unbox Ply
- New functions: rank, file, rankChar, fileChar, toCoord
- New module: Game.Chess.ECO
- Chess.Game.UCI: PV is now an unboxed vector
- Remove IsSquare class add three newtye'd Ints: Square, Rank, File

## chessIO 0.6.1.1

- Fix [half-move counter not being reset on pawn push or capture](https://github.com/mlang/chessIO/issues/2) (Thanks to Oleg Grenrus)
- Fix [toFEN not emitting a dash when only king moved](https://github.com/mlang/chessIO/issues/3) (Thanks to Oleg Grenrus)

## chessIO 0.6.1.0

- Add enPassantSquare (thanks Tochi Obudulu).
- cbookview:
  - piece styles
  - Key End now moves to the final position of the most popular game
  - FEN string display

## chessIO 0.6.0.0

- Optimize `foldBits`.
- Avoid cycles in `bookForest`.
- Don't reexport tree related functions from Game.Chess.
- Split SAN functions into new exposed module Chess.Game.SAN.
- Rename `Game.Chess.Polyglot.Book` to `Game.Chess.Polyglot`.
- New functions `plySource` and `plyTarget`.
- New tool `cbookview`: terminal chess book opening explorer.

## chessIO 0.5.0.0

- Split SAN parsing code into a separate module.
- Adapt to VisualStream change in Megaparsec >= 9.
- Use Maybe to indicate that bestmove in UCI can be empty.
- instance Storable QuadBitboard

## chessIO 0.4.0.0

- Support for letting UCI engines ponder.
- Avoid a branch to further speed up move generation.

