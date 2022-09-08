{-# LANGUAGE BinaryLiterals #-}
module Main where

import System.Exit (exitSuccess, die)
import System.Posix.Terminal
    (getTerminalAttributes,
    setTerminalAttributes,
    withoutMode,
    TerminalMode(ProcessInput, EnableEcho, KeyboardInterrupts, StartStopOutput, ExtendedFunctions, MapCRtoLF, ProcessOutput, InterruptOnBreak, CheckParity, StripHighBit),
    TerminalState(Immediately), TerminalAttributes, withMinInput, withTime)
import System.Posix.Types (Fd)
import Data.Char (isControl, generalCategory, ord)
import Data.List (foldl')
import System.Timeout (timeout)
import Control.Monad (when, forever, replicateM_, forM_)
import Data.Maybe (isNothing, fromJust, isJust)
import Data.Bits ((.&.))
import System.Console.ANSI (clearScreen, setCursorPosition, reportCursorPosition, getTerminalSize, saveCursor, getCursorPosition)
import System.IO (hReady, stdin, stdout, hShow, hFlush, hGetLine)
import Debug.Trace (trace)

stdinFd :: Fd
stdinFd = 0

withoutModes :: [TerminalMode] -> TerminalAttributes  -> TerminalAttributes
withoutModes = flip (foldl' withoutMode)

timeout' :: Int -> IO Char -> (Char -> IO ()) -> IO () -> IO ()
timeout' t action handler errorAction = do
    result <- timeout t action
    maybe errorAction handler result

handler :: Char -> IO ()
handler c
  | isControl c = handlerControl c 
  | otherwise   = trace "is not control" return () 


handlerControl :: Char -> IO ()
handlerControl c
  | ctrlKey c == 17 = clearScreen >> exitSuccess 
  | otherwise       = print (ord c) >> putChar '\r'

ctrlKey :: Char -> Int
ctrlKey = (.&. 0b00011111) . ord

initWindow :: (Int, Int) -> IO ()
initWindow (h, _) = do
    clearScreen
    replicateM_ (fromIntegral h - 1) $ putStr "~\r\n"
    putStr "~"
    setCursorPosition 0 0
    -- saveCursor


main :: IO ()
main = do
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
    hShow stdin >>= print
    putStr "\r\n"
    getChar >>= print
    pos <- getCursorPosition
    -- windowSize <- getTerminalSize 
    hShow stdin >>= print
    getChar >>= print
    -- forM_ windowSize print
    -- maybe (die "No window size") initWindow windowSize
    -- forever $ timeout' 1000000 getChar handler (putStr "hello\r\n")
