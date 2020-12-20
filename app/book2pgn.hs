import Game.Chess
import Game.Chess.PGN
import Game.Chess.Polyglot.Book
import System.IO

main :: IO ()
main = hPutPGN stdout depthFirst $ toPGN defaultBook startpos
