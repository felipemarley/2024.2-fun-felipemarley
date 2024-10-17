module Delusion where

flip :: (a -> b -> c) -> b -> a -> c
flip f y x = f x y 

idenelse :: Bool -> a -> a -> a
idenelse True x _ = x
idenelse _ _ y = y

