module Him.KeyCode (
  KeyCode(..)
  , c2k
) where

import Data.Bits ((.&.))
import Data.Char
    (isControl, generalCategory, ord, GeneralCategory (Control))

data KeyCode =
    CTRL_Q
  | LA --a
  | LS --s
  | LD --d
  | LW --w 
  | LH --h
  | LJ --j
  | LK --k
  | LL --l
  | UNKNOWN deriving (Show, Eq)


ctrlKey :: Char -> Int
ctrlKey = (.&. 0b00011111) . ord

c2k :: Char -> KeyCode
c2k c
  | isControl c = c2kCtrl $ ctrlKey c
  | c == 'a'    = LA
  | c == 's'    = LS
  | c == 'd'    = LD
  | c == 'w'    = LW
  | c == 'h'    = LH
  | c == 'j'    = LJ
  | c == 'k'    = LK
  | c == 'l'    = LL
  | otherwise   = UNKNOWN


c2kCtrl :: Int -> KeyCode
c2kCtrl 17 = CTRL_Q 
c2kCtrl _  = UNKNOWN
