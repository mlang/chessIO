module Game.Chess.UCI (
  Engine, name, author, options
, start, start'
, Option(..), getOption, setOptionSpinButton
, quit, quit'
) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Ix
import Data.Maybe
import Data.String
import System.Exit
import System.IO
import System.Process
import System.Timeout (timeout)

data Engine = Engine {
  inH :: Handle
, outH :: Handle
, procH :: ProcessHandle
, name :: Maybe ByteString
, author :: Maybe ByteString
, options :: HashMap ByteString Option
}

data Command = Name ByteString
             | Author ByteString
             | Option ByteString Option
             | UCIOk | ReadyOK
             deriving (Show)

data Option = CheckBox Bool
            | ComboBox { comboBoxValue :: ByteString, comboBoxValues :: [ByteString] }
            | SpinButton { spinButtonValue, spinButtonMinBound, spinButtonMaxBound :: Int }
            | String ByteString
            | Button
            deriving (Eq, Show)

instance IsString Option where
  fromString = String . BS.pack

command :: Parser Command
command = skipSpace *> choice [name, author, option, uciok, readyok] <* skipSpace
 where
  name = fmap Name $
    "id" *> skipSpace *> "name" *> skipSpace *> takeByteString
  author = fmap Author $
    "id" *> skipSpace *> "author" *> skipSpace *> takeByteString
  option = do
    void "option"
    skipSpace
    void "name"
    skipSpace
    optName <- BS.pack <$> manyTill anyChar (skipSpace *> "type")
    skipSpace
    optValue <- spin <|> check <|> combo <|> str <|> button
    pure $ Option optName optValue
  check =
    fmap CheckBox $ "check" *> skipSpace *> "default" *> skipSpace *>
                    ("false" $> False <|> "true" $> True)
  spin = do
    void "spin"
    skipSpace
    value <- "default" *> skipSpace *> signed decimal <* skipSpace
    minValue <- "min" *> skipSpace *> signed decimal <* skipSpace
    maxValue <- "max" *> skipSpace *> signed decimal
    pure $ SpinButton value minValue maxValue
  combo = do
    void "combo"
    skipSpace
    def <- fmap BS.pack $ "default" *> skipSpace *> manyTill anyChar var
    (vars, lastVar) <- (,) <$> many (manyTill anyChar var)
                           <*> takeByteString
    pure $ ComboBox def (map BS.pack vars <> [lastVar])
  var = skipSpace *> "var" *> skipSpace
  str = fmap String $
    "string" *> skipSpace *> "default" *> skipSpace *> takeByteString
  button = "button" $> Button
  uciok = "uciok" $> UCIOk
  readyok = "readyok" $> ReadyOK

start :: String -> [String] -> IO (Maybe Engine)
start = start' 2000000

start' :: Int -> String -> [String] -> IO (Maybe Engine)
start' usec cmd args = do
  (Just inH, Just outH, Nothing, procH) <- createProcess (proc cmd args) {
      std_in = CreatePipe, std_out = CreatePipe
    }
  hSetBuffering inH LineBuffering
  let c = Engine inH outH procH Nothing Nothing HashMap.empty
  send "uci" c
  timeout usec (initialise c) >>= \case
    Just c' -> pure . Just $ c'
    Nothing -> quit c >> pure Nothing

initialise :: Engine -> IO Engine
initialise c@Engine{outH} = do
  l <- BS.hGetLine outH
  case parseOnly (command <* endOfInput) l of
    Left err -> do
      putStrLn err
      print l
      initialise c
    Right (Name n) -> initialise (c { name = Just n })
    Right (Author a) -> initialise (c { author = Just a })
    Right (Option name opt) -> initialise (c { options = HashMap.insert name opt $ options c })
    Right UCIOk -> pure c

send :: ByteString -> Engine -> IO ()
send s Engine{inH, procH} = do
  BS.hPutStrLn inH s
  getProcessExitCode procH >>= \case
    Nothing -> pure ()
    Just ec -> throwIO ec

getOption :: ByteString -> Engine -> Maybe Option
getOption n = HashMap.lookup n . options

setOptionSpinButton :: ByteString -> Int -> Engine -> IO Engine
setOptionSpinButton n v c
  | Just (SpinButton _ minValue maxValue) <- getOption n c
  , inRange (minValue, maxValue) v
  = do
    send ("setoption name " <> n <> " value " <> BS.pack (show v)) c
    pure $ c { options = HashMap.update (set v) n $ options c }
 where
  set v opt@SpinButton{} = Just $ opt { spinButtonValue = v }

quit :: Engine -> IO (Maybe ExitCode)
quit = quit' 1000000

quit' :: Int -> Engine -> IO (Maybe ExitCode)
quit' usec c@Engine{procH} = do
  (pure . Just) `handle` do
    send "quit" c
    timeout usec (waitForProcess procH) >>= \case
      Just ec -> pure $ Just ec
      Nothing -> terminateProcess procH $> Nothing
