# A Haskell chess library and console UCI frontend program

`chessIO` is a Haskell library for working with chess positions and moves,
and a console frontend program (cboard) to work with UCI compatible
chess engines.

## The Library

The main module provided by the library is
[Game.Chess](https://hackage.haskell.org/package/chessIO/docs/Game-Chess.html),
which defines data types and functions for working with chess positions
and moves. It offers a fully compliant move generator and parsing for
and printing positions in [Forsyth-Edwards
Notation](https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation)
and moves in [Algebraic
Notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)).

Module
[Game.Chess.UCI](https://hackage.haskell.org/package/chessIO/docs/Game-Chess-UCI.html)
provides functionality to run an external process which understands the
Universal Chess Interface protocol from within Haskell.

## Console frontend for the Universal Chess Interface protocl

`cboard` is a simple console (text-mode) frontend for interacting with
chess engines (like stockfish or glaurung) which make use of the UCI
protocol.

To launch a chess engine, simply pass its executable name and arguments
to cboard. For instance, `cboard stockfish`.

## Opening book explorer

`cbookview` is a terminal application to explore commonly played openings.
Passing a polyglot opening book file (with extension `.bin`) on the command line
will allow you to explore the plies contained in that book file interactively.
You can also open a PGN file (extension `.pgn`) which will be presented like
it was an opening book.  In other words, all the moves played in that PGN file will
be merged into a single forest of plies.  When exporing PGN files, no
particular order of plies is imposed.  When exploring a polyglot file
the most popular moves will always come first.

For example, lets assume you want to examine blacks replies to
the [Ruy lopez](https://en.wikipedia.org/wiki/Ruy_Lopez).  Use the cursor keys
to navigate to the move after bishop b5.
Here is what the interface will show:

```
a6         ┌─────────────────┐
Nf6       1│ R +   K Q B N R │        1.e4 e5 2.Nf3 Nc6 3.Bb5 a6
g6        2│ P P P   P P P P │
f5        3│   + N +   +   + │
Nge7      4│ +   + P +   +   │
Bc5       5│   +   p   + B + │
d6        6│ +   +   + n + p │
Nd4       7│ p p p + p p p + │
Bb4       8│ r n b k q b + r │
Bd6        └─────────────────┘
Qf6          h g f e d c b a
f6



Up/Down (kj) = change ply, Left/Right (hl) = back/forward                 ESC = Quit```
```

Moving the cursor down the list of book moves will also update the
position and game history.  As already mentioned, the moves in polyglot book files
are ordered according to popularity.  So pawn to a6 is actually the
most popular line of the Ruy Lopez.

### The easteregg: Poor mans chessboard

If you press `a` (for "All moves") cbookview will switch to the tree
of all possible moves.  This is a poor mans way for following
games and abusing cbookview as a two-player board.
