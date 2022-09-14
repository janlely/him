module Main where

import System.Environment
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
    getTerminalSize, getCursorPosition)
import System.IO (hReady, stdin, stdout, hShow, hFlush, hGetLine)
import Debug.Trace (trace)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Him.Key (KeyCode(..), c2k, getKey, getAction)
import Data.Word (Word8)
import Him.Args (parseCommandLineArgs)
import Him.State (HimState, getMode, emptyHimState, initHimState, HimMode(..))
import qualified Him.Action as HA



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
    case getMode hs' of
        HimNormal -> normalModeHandler key hs
        HimInsert -> insertModeHandler key hs
        HimSelect -> selectModeHandler key hs

normalModeHandler :: KeyCode -> IORef HimState -> IO ()
normalModeHandler = HA.exeAction . getAction


insertModeHandler = error "to be implemented"

selectModeHandler = error "to be implemented"


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
    when (isNothing windowSize) (die "No window size")
    enterRawMode
    hs <- newIORef $ emptyHimState (fromJust windowSize)
    args <- parseCommandLineArgs
    initHimState args hs
    forever $ do
        key <- getKey
        handler hs key
