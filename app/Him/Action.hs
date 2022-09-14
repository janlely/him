module Him.Action  where

import Data.IORef (IORef, modifyIORef, readIORef)
import Him.State (HimState(..), getXPos, getYPos, xPosUp, xPosDown, yPosUp, yPosDown, render
    , getScreenHeight, getScreenWidth)
import System.Console.ANSI (clearScreen, cursorUp, cursorDown, cursorBackward, cursorForward, setCursorPosition)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Debug.Trace (trace)
import System.Exit (exitSuccess)

data Action = Action { _actionName :: String
                     , _actionDescription :: String
                     , _condition :: HimState -> Bool
                     , _refresh :: Bool
                     , _action :: IORef HimState -> IO ()
                     }

exeAction :: Action -> IORef HimState -> IO ()
exeAction action hs = do
    hs' <- readIORef hs
    when (_condition action hs') $ do
        -- putStr $ _actionName action ++ "fired\r\n"
        _action action hs
        when (_refresh action) $ render hs
        hFlush stdout

undefinedAction :: Action
undefinedAction = Action {
    _actionName = "undefined action"
  , _actionDescription = "undefined action"
  , _condition = const False
  , _refresh = False
  , _action = \_ -> return ()
}

cursorUpAction :: Action
cursorUpAction = Action {
    _actionName = "cursorUp"
  , _actionDescription = "Move the cursor up"
  , _condition = (> 0) . getXPos 
  , _refresh = False
  , _action = \state -> do
        s <- readIORef state
        let (x, y) = (getXPos s, getYPos s) 
        setCursorPosition (x - 1) y
        modifyIORef state xPosUp
  }

cursorDownAction :: Action
cursorDownAction = Action {
    _actionName = "cursorDown"
  , _actionDescription = "Move the cursor down"
  , _condition = \state -> getXPos state < getScreenHeight state
  , _refresh = False
  , _action = \state -> do
        s <- readIORef state
        let (x, y) = (getXPos s, getYPos s) 
        setCursorPosition (x + 1) y
        modifyIORef state xPosDown
  } 

cursorLeftAction :: Action
cursorLeftAction = Action {
    _actionName = "cursorLeft"
  , _actionDescription = "Move the cursor left"
  , _condition = (> 0) . getYPos
  , _refresh = False
  , _action = \state -> do
        s <- readIORef state
        let (x, y) = (getXPos s, getYPos s) 
        setCursorPosition x (y - 1)
        modifyIORef state yPosUp
  }

cursorRightAction :: Action
cursorRightAction = Action {
    _actionName = "cursorLeft"
  , _actionDescription = "Move the cursor left"
  , _condition = \state -> getYPos state < getScreenWidth state
  , _refresh = False
  , _action = \state -> do
        s <- readIORef state
        let (x, y) = (getXPos s, getYPos s) 
        setCursorPosition x (y + 1)
        modifyIORef state yPosDown
  }

quit :: Action
quit = Action {
    _actionName = "quit"
  , _actionDescription = "Quit the editor"
  , _condition = const True
  , _refresh = False
  , _action = \_ -> setCursorPosition 0 0 >> clearScreen >> exitSuccess
  }