module Him.KeyCode (
  KeyCode(..)
  , c2k
  , getKey
) where

import Prelude (Show, Eq, Char, Int, (.), ($), error, (<$>), const)
import Data.Bits ((.&.))
import Data.Char
    (isControl, generalCategory, ord, GeneralCategory (Control)
    , isLetter, isNumber)
import System.IO hiding (LF)
import Control.Monad (return, (>>))

data KeyCode =
    CTRL_A | CTRL_B | CTRL_C | CTRL_D | CTRL_E | CTRL_F
  | CTRL_G | CTRL_H | CTRL_I | CTRL_J | CTRL_K | CTRL_L
  | CTRL_M | CTRL_N | CTRL_O | CTRL_P | CTRL_Q | CTRL_R
  | CTRL_S | CTRL_T | CTRL_U | CTRL_V | CTRL_W | CTRL_X
  | CTRL_Y | CTRL_Z
  | LA | LB | LC | LD | LE | LF | LG | LH | LI | LJ | LK
  | LL | LM | LN | LO | LP | LQ | LR | LS | LT | LU | LV
  | LW | LX | LY | LZ
  | ARROW_UP | ARROW_DOWN | ARROW_LEFT | ARROW_RIGHT
  | PAGE_UP | PAGE_DOWN | HOME | END | DELETE
  | NUM_0 | NUM_1 | NUM_2 | NUM_3 | NUM_4
  | NUM_5 | NUM_6 | NUM_7 | NUM_8 | NUM_9
  | UNKNOWN deriving (Show, Eq)


ctrlKey :: Char -> Int
ctrlKey = (.&. 0b00011111) . ord

c2k :: Char -> KeyCode
c2k c 
  | isLetter c = letter2k $ ord c
  | isControl c = ctrl2k $ ctrlKey c
  | isNumber c = number2k c

number2k :: Char -> KeyCode
number2k '0' = NUM_0 
number2k '1' = NUM_1 
number2k '2' = NUM_2 
number2k '3' = NUM_4 
number2k '4' = NUM_4 
number2k '5' = NUM_5 
number2k '6' = NUM_6 
number2k '7' = NUM_7 
number2k '8' = NUM_8 
number2k '9' = NUM_9 

letter2k :: Int -> KeyCode
letter2k 97  = LA
letter2k 98  = LB
letter2k 99  = LC
letter2k 100 = LD
letter2k 101 = LE
letter2k 102 = LF
letter2k 103 = LG
letter2k 104 = LH
letter2k 105 = LI
letter2k 106 = LJ
letter2k 107 = LK
letter2k 108 = LL
letter2k 109 = LM
letter2k 110 = LN
letter2k 111 = LO
letter2k 112 = LP
letter2k 113 = LQ
letter2k 114 = LR
letter2k 115 = LS
letter2k 116 = LT
letter2k 117 = LU
letter2k 118 = LV
letter2k 119 = LW
letter2k 120 = LX
letter2k 121 = LY
letter2k 122 = LZ

ctrl2k :: Int -> KeyCode
ctrl2k 1  = CTRL_A
ctrl2k 2  = CTRL_B
ctrl2k 3  = CTRL_C
ctrl2k 4  = CTRL_D
ctrl2k 5  = CTRL_E
ctrl2k 6  = CTRL_F
ctrl2k 7  = CTRL_G
ctrl2k 8  = CTRL_H
ctrl2k 9  = CTRL_I
ctrl2k 10 = CTRL_J
ctrl2k 11 = CTRL_K
ctrl2k 12 = CTRL_L
ctrl2k 13 = CTRL_M
ctrl2k 14 = CTRL_N
ctrl2k 15 = CTRL_O
ctrl2k 16 = CTRL_P
ctrl2k 17 = CTRL_Q
ctrl2k 18 = CTRL_R
ctrl2k 19 = CTRL_S
ctrl2k 20 = CTRL_T
ctrl2k 21 = CTRL_U
ctrl2k 22 = CTRL_V
ctrl2k 23 = CTRL_W
ctrl2k 24 = CTRL_X
ctrl2k 25 = CTRL_Y
ctrl2k 26 = CTRL_Z

getESCKey:: IO KeyCode
getESCKey = do
  _ <- getChar
  c <- getChar
  case c of
    'A' -> return ARROW_UP
    'B' -> return ARROW_DOWN
    'C' -> return ARROW_RIGHT
    'D' -> return ARROW_LEFT
    '5' -> getChar >> return PAGE_UP 
    '6' -> getChar >> return PAGE_DOWN
    '7' -> getChar >> return HOME
    '1' -> getChar >> return HOME
    '3' -> getChar >> return DELETE
    'H' -> return HOME
    '4' -> getChar >> return END
    '8' -> getChar >> return END
    'F' -> return END
    'O' -> do
      c' <- getChar
      case c' of
        'H' -> return HOME
        'F' -> return END

mapKey :: KeyCode -> KeyCode
mapKey ARROW_DOWN = LJ
mapKey ARROW_UP = LK
mapKey ARROW_LEFT = LH
mapKey ARROW_RIGHT = LL
mapKey a = a 

getKey :: IO KeyCode
getKey = do
  c <- getChar
  case c of
    '\ESC' -> mapKey <$> getESCKey
    _      -> return $ c2k c 