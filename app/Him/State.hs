module Him.State (
  HimState(..)
  , HimMode (..)
  , getMode
  , xPosUp , xPosDown, yPosUp , yPosDown
  , getXPos, getYPos
  , emptyHimState, initHimState
  , render
  , getScreenHeight, getScreenWidth
) where

import qualified Control.Lens as LEN
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (IORef, modifyIORef, readIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Him.Args (CommondLineArgs, getFilePath)
import Data.Maybe (maybe, isJust, fromJust)
import qualified Data.Map.Strict as M
import Debug.Trace (trace)
import System.IO (hFlush, stdout)
import System.Console.ANSI
    (clearScreen,setCursorPosition)
import Control.Monad (when, forever, replicateM_, forM_)
import Him.FileType (FileType)

data HimMode = HimInsert | HimNormal | HimSelect deriving (Show)
data HimState = HimState
  { _xPos :: !Int
  , _yPos :: !Int
  , _screenHeight :: !Int
  , _screenWidth :: !Int
  , _rows :: !(V.Vector HimRow) 
  , _currentRange :: !(Int, Int)
  , _lineMap :: !(M.Map Int Int)
  , _himMode :: HimMode
  , _fileType :: FileType}

-- instance Show HimState where
--     show (HimState x y sr sc _ (r1,r2) _ m) =
--         L.intercalate ""  (show <$> [x, y, sr, sc, r1, r2]) ++ " " ++ show m

data HimRow = HimRow
  { _content:: !BS.ByteString
  , _len  :: !Int}

$(LEN.makeLenses ''HimState)
$(LEN.makeLenses ''HimRow)

getMode :: HimState -> HimMode
getMode = _himMode

emptyHimState :: (Int, Int) -> HimState
emptyHimState (h,w) = HimState 0 0 h w V.empty (0,0) M.empty HimNormal "txt"


printWelcomeMessage :: (Int, Int) -> IO ()
printWelcomeMessage (h, w) = do
    replicateM_ ((h - 1) `div` 2) $ putStr "\r\n"
    let welcome = "welcome to him"
        len = length welcome
    when (len < w) $ do
        replicateM_ ((w - len) `div` 2) $ putChar ' '
    putStr welcome
    putStr "\r\n"

initWindow :: IORef HimState -> IO ()
initWindow hs = do
  hs' <- readIORef hs
  let h = LEN.view screenHeight hs'
      w = LEN.view screenWidth hs'
  clearScreen
  replicateM_ (h-1) $ putStr "~\r\n"
  putStr "~\r"
  setCursorPosition 0 0
  printWelcomeMessage (w,h)
  setCursorPosition 0 0
  hFlush stdout

initHimState :: CommondLineArgs -> IORef HimState -> IO ()
initHimState args hs = do
  case getFilePath args of
    Nothing -> initWindow hs
    Just path -> do
      lines <- parseRows <$> BS.readFile path
      modifyIORef hs (updateRange . updateRows lines)
  render hs

updateRows :: V.Vector HimRow -> HimState -> HimState
updateRows = LEN.set rows

updateRange :: HimState -> HimState
updateRange hs = let minLen = min (LEN.view screenHeight hs) (V.length $ LEN.view rows hs)
                     lineMapList = zip [0,1..minLen]  [0,1..minLen]
                  in LEN.set currentRange (0, minLen) . LEN.set lineMap (M.fromList lineMapList) $ hs


      

parseRows :: BS.ByteString -> V.Vector HimRow
parseRows = V.fromList . fmap (\line -> HimRow line (count line)) . BSC.lines 
  where count = T.length . T.decodeUtf8

xPosUp :: HimState -> HimState
xPosUp = LEN.over xPos (\x -> if x > 0 then x-1 else x) 

xPosDown :: HimState -> HimState
xPosDown = LEN.over xPos (+1)

yPosUp :: HimState -> HimState
yPosUp = LEN.over yPos (\x -> if x > 0 then x-1 else x)

yPosDown :: HimState -> HimState
yPosDown = LEN.over yPos (+1)

getXPos :: HimState -> Int
getXPos = _xPos

getYPos :: HimState -> Int
getYPos = _yPos

getScreenHeight :: HimState -> Int
getScreenHeight = _screenHeight

getScreenWidth :: HimState -> Int
getScreenWidth = _screenWidth

renderRows :: (Int, Int) -> V.Vector HimRow -> IO ()
renderRows (start, length) = T.putStr . T.decodeUtf8 . BS.intercalate (BSC.pack "\r\n") . V.toList . V.map _content . V.slice start length   

render :: IORef HimState -> IO ()
render hs = do
  hs' <- readIORef hs
  renderRows (LEN.view currentRange hs') (LEN.view rows hs')
  setCursorPosition 0 0 
  -- updateCursor hs
  hFlush stdout

-- updateCursor :: IORef HimState -> IO ()
-- updateCursor hs = do
--   currentPos <- getCursorPosition 
--   when (isJust currentPos) $ do
--     let (x,y) = fromJust currentPos
--     modifyIORef hs (LEN.set xPos x . LEN.set yPos y)
