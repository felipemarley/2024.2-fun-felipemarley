module Delusion where

--loucura que joao lucas me forçou a aprender a fazer 
flip :: (a -> b -> c) -> b -> a -> c
flip f y x = f x y 


--if then else caseiro
idenelse :: Bool -> a -> a -> a
idenelse True x _ = x
idenelse _ _ y = y

