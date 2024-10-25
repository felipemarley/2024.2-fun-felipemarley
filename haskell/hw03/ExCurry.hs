module ExCurry where

import Prelude hiding ( curry , uncurry )

-- use your mind to infer the types, don't cheat!

-- curry gets a "traditional" binary function
-- and returns its currified version
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

-- uncurry gets a currified function
-- and returns its "traditional" binary version
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y 

{-
addTuple :: (Int, Int) -> Int
addTuple (x, y) = x + y

addCurried :: Int -> Int -> Int
addCurried x y = x + y

main :: IO ()
main = do
    print (uncurry (curry addTuple) (3, 4)) -- Deve retornar 7
    print (curry (uncurry addCurried) 3 5)  -- Deve retornar 7
    -}
