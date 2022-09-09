module Him.Cursor (
  moveCursorUp, moveCursorDown, moveCursorLeft, moveCursorRight
  , updateCursorPosition
) where

import Data.IORef (IORef, modifyIORef)
import Him.State (HimState, xPosUp, xPosDown, yPosUp, yPosDown, getXPos, getYPos)
import System.IO (hFlush, stdout)
import System.Console.ANSI (cursorUp, cursorDown, cursorBackward, cursorForward, setCursorPosition)


moveCursorUp :: IORef HimState -> IO ()
moveCursorUp hs = do
  modifyIORef hs xPosUp
  cursorUp 1
  hFlush stdout

moveCursorDown :: IORef HimState -> IO ()
moveCursorDown hs = do
    modifyIORef hs xPosDown 
    cursorDown 1
    hFlush stdout

moveCursorLeft :: IORef HimState -> IO ()
moveCursorLeft hs = do
    modifyIORef hs yPosUp
    cursorBackward 1
    hFlush stdout

moveCursorRight :: IORef HimState -> IO ()
moveCursorRight hs = do
    modifyIORef hs yPosDown 
    cursorForward 1
    hFlush stdout

updateCursorPosition :: HimState -> IO ()
updateCursorPosition hs = do
    setCursorPosition (getXPos hs) (getYPos hs)
    hFlush stdout