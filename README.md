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
Passing a polyglot opening book file (with extension `.bin`) will allow you to
explore the plies contained in that book file interactively.
You can also open a PGN file (extension `.pgn`) which will be presented like
it was an opening book.  In other words, all the moves played in that PGN file will
be merged into a single forest of plies.  When exporing PGN files, no
particular order of plies is imposed.  When exploring a polyglot file
the most popular moves will always come first.
