module Him.Util where

map2 :: (a -> b) -> (a -> c) -> (b -> c -> d) -> a -> d
map2 f g h x = h (f x) (g x)