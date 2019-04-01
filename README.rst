chessIO -- A Haskell chess library and console UCI frontend program
===================================================================

chessIO is a Haskell library for working with chess positions and moves, and a
console frontend program (cboard) to work with UCI compatible chess engines.

The Library
-----------

The main module provided by the library is Game.Chess_, which defines
data types and functions for working with chess positions and moves.
It offers a fully compliant move generator and parsing for and printing
positions in `Forsyth-Edwards Notation`_ and moves in `Algebraic Notation`_.

Module Game.Chess.UCI_ provides functionality to run an external process
which understands the Universal Chess Interface protocol from within Haskell.

cboard -- Console frontend for the Universal Chess Interface protocl
--------------------------------------------------------------------

cboard is a simple console (text-mode) frontend for interacting with chess engines
(like stockfish or glaurung) which make use of the UCI protocol.

To launch a chess engine, simply pass its executable name and arguments
to cboard.  For instance, `cboard stockfish`.

.. _`Forsyth-Edwards Notation`: https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
.. _`Algebraic Notation`: https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
.. _Game.Chess: https://hackage.haskell.org/package/chessIO/docs/Game-Chess.html
.. _Game.Chess.UCI: https://hackage.haskell.org/package/chessIO/docs/Game-Chess-UCI.html
