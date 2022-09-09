module Him.State (
  HimState(..)
  , HimMode (..)
  , getMode
  , xPosUp , xPosDown, yPosUp , yPosDown
  , getXPos, getYPos
  , emptyHimState, initHimState
) where

import Control.Lens ( over, makeLenses, view , set)
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.IORef (IORef, modifyIORef)
import Data.Text as T
import qualified Data.Text.Encoding as T
import Him.Args (CommondLineArgs, getFilePath)
import Data.Maybe (maybe, isJust)
import Control.Monad (when)

data HimMode = HimInsert | HimNormal | HimSelect deriving (Show)
data HimState = HimState
  { _xPos :: !Int
  , _yPos :: !Int
  , _screenRows :: !Int
  , _screenCols :: !Int
  , _rows :: !(V.Vector HimRow) 
  , _himMode :: HimMode}

instance Show HimState where
    show (HimState x y sr sc _ m) =
        L.intercalate ""  (show <$> [x, y, sr, sc]) ++ " " ++ show m

data HimRow = HimRow
  { _content:: !BS.ByteString
  , _len  :: !Int}

$(makeLenses ''HimState)
$(makeLenses ''HimRow)

getMode :: HimState -> HimMode
getMode = _himMode

emptyHimState :: HimState
emptyHimState = HimState 0 0 0 0 V.empty HimNormal

initHimState :: CommondLineArgs -> IORef HimState -> IO ()
initHimState args hs =
  case getFilePath args of
    Nothing -> return ()
    Just path -> do
      rows <- parseRows <$> BS.readFile path
      modifyIORef hs $ updateRows rows

updateRows :: V.Vector HimRow -> HimState -> HimState
updateRows = set rows

parseRows :: BS.ByteString -> V.Vector HimRow
parseRows = V.fromList . fmap (\line -> HimRow line (count line)) . BS.lines 
  where count = T.length . T.decodeUtf8

xPosUp :: HimState -> HimState
xPosUp = over xPos (\x -> if x > 1 then x-1 else x) 

xPosDown :: HimState -> HimState
xPosDown = over xPos (+1)

yPosUp :: HimState -> HimState
yPosUp = over yPos (\x -> if x > 1 then x-1 else x)

yPosDown :: HimState -> HimState
yPosDown = over yPos (+1)

getXPos :: HimState -> Int
getXPos = _xPos

getYPos :: HimState -> Int
getYPos = _yPos