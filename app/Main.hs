module Main where

import System.Exit (exitSuccess, die)
import System.Posix.Terminal
    (getTerminalAttributes,
    setTerminalAttributes,
    withoutMode,
    TerminalMode(ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput, ExtendedFunctions, MapCRtoLF, ProcessOutput, InterruptOnBreak, CheckParity, StripHighBit),
    TerminalState(Immediately), TerminalAttributes, withMinInput, withTime)
import System.Posix.Types (Fd)
import Data.List (foldl')
import System.Timeout (timeout)
import Control.Monad (when, forever, replicateM_, forM_)
import Data.Maybe (isNothing, fromJust, isJust, catMaybes)
import System.Console.ANSI
    (clearScreen,setCursorPosition,reportCursorPosition,
    getTerminalSize, saveCursor, getCursorPosition, cursorUp,
    cursorDown, cursorForward, cursorBackward)
import System.IO (hReady, stdin, stdout, hShow, hFlush, hGetLine)
import Debug.Trace (trace)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Lens ( over, makeLenses, view )
import Him.KeyCode (KeyCode(..), c2k, getKey)


data HimMode = HimInsert | HimNormal | HimSelect deriving (Show)

data HimState = HimState
    { _xPos :: Int
    , _yPos :: Int
    , _himMode :: HimMode} deriving (Show)

$(makeLenses ''HimState)

initHimState :: HimState
initHimState = HimState 0 0 HimNormal

stdinFd :: Fd
stdinFd = 0

withoutModes :: [TerminalMode] -> TerminalAttributes  -> TerminalAttributes
withoutModes = flip (foldl' withoutMode)

-- timeout' :: Int -> IO Char -> (Char -> IO ()) -> IO () -> IO ()
-- timeout' t action handler errorAction = do
--     result <- timeout t action
--     maybe errorAction handler result

handler :: IORef HimState -> KeyCode -> IO ()
handler hs key = do
    hs' <- readIORef hs 
    case _himMode hs' of
        HimNormal -> normalModeHandler hs key
        HimInsert -> insertModeHandler hs key
        HimSelect -> selectModeHandler hs key

normalModeHandler :: IORef HimState -> KeyCode -> IO ()
normalModeHandler hs c 
  | c == CTRL_Q = clearScreen >> exitSuccess 
  | c == LK     = moveCursorUp hs 
  | c == LJ     = moveCursorDown hs
  | c == LH     = moveCursorLeft hs
  | c == LL     = moveCursorRight hs
  | otherwise   = putChar '\r'



moveCursorUp :: IORef HimState -> IO ()
moveCursorUp hs = do
    hs' <- readIORef hs
    writeIORef hs (over xPos (\x -> if x > 1 then x-1 else x) hs')
    cursorUp 1
    hFlush stdout

moveCursorDown :: IORef HimState -> IO ()
moveCursorDown hs = do
    hs' <- readIORef hs
    writeIORef hs (over xPos (+1) hs')
    cursorDown 1
    hFlush stdout

moveCursorLeft :: IORef HimState -> IO ()
moveCursorLeft hs = do
    hs' <- readIORef hs
    writeIORef hs (over yPos (\x -> if x > 1 then x-1 else x) hs')
    cursorBackward 1
    hFlush stdout

moveCursorRight :: IORef HimState -> IO ()
moveCursorRight hs = do
    hs' <- readIORef hs
    writeIORef hs (over yPos (+1) hs')
    cursorForward 1
    hFlush stdout


updateCursorPosition :: HimState -> IO ()
updateCursorPosition hs = do
    setCursorPosition (view xPos hs) (view yPos hs)
    hFlush stdout


insertModeHandler = error "to be implemented"

selectModeHandler = error "to be implemented"



initWindow :: (Int, Int) -> IO ()
initWindow s@(h, _) = do
    clearScreen
    replicateM_ (h-1) $ putStr "~\r\n"
    putStr "~\r"
    setCursorPosition 0 0
    printWelcomeMessage s
    setCursorPosition 0 0
    hFlush stdout

printWelcomeMessage :: (Int, Int) -> IO ()
printWelcomeMessage (h, w) = do
    replicateM_ ((h - 1) `div` 2) $ putStr "\r\n"
    let welcome = "welcome to him"
        len = length welcome
    when (len < w) $ do
        replicateM_ ((w - len) `div` 2) $ putChar ' '
    putStr welcome
    putStr "\r\n"

enterRawMode :: IO ()
enterRawMode = do
    termAttr <- getTerminalAttributes stdinFd
    setTerminalAttributes stdinFd
        (withoutModes
            [EnableEcho
            , ProcessInput
            , KeyboardInterrupts
            , StartStopOutput
            , ExtendedFunctions
            , MapCRtoLF
            , InterruptOnBreak
            , CheckParity
            , StripHighBit
            , ProcessOutput] termAttr) Immediately


main :: IO ()
main = do
    windowSize <- getTerminalSize 
    forM_ windowSize print
    maybe (die "No window size") initWindow windowSize
    enterRawMode
    hs <- newIORef initHimState 
    forever $ do
        key <- getKey
        print key
        handler hs key

    -- forever $ getChar >>= handler hs
    -- forever $ timeout' 1000000 getChar handler (putStr "hello\r\n")
