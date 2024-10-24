{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )
--my version of "if-then-else"  
import Delusion   

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
    show O = "O"
    show (S O) = "SO"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) O O = True
    (==) (S n) (S m) = (==) n m
    (==) _ _ = False

instance Ord Nat where
    
    (<=) O _ = True
    (<=) _ O = False
    (<=) (S n) (S m) = (<=) n m

    min _ O = O
    min O _ = O
    min (S n) (S m) = S (min n m)

    max n O = n
    max O n = n
    max (S n) (S m) = S (max n m)


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred (S n) = n
pred _ = O

even :: Nat -> Bool
even n = idenelse (n <|> sso) True False

odd :: Nat -> Bool
odd n = idenelse (even n) False True


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
S n <-> S m = n <-> m
n <-> _ = n

-- multiplication
(<*>) :: Nat -> Nat -> Nat 
n <*> O = O
n <*> S m = n <+> (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
_ <^> O = S O
O <^> _ = O
n <^> (S m) = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "couldnt div 0"
O </> _ = O 
n </> m = idenelse (n >= m) (S ((n <-> m) </> m)) O

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = error "couldnt div 0"
O <%> _ = O 
n <%> m = idenelse (n >= m) ((n <-> m) <%> m) n

-- divides
--need fix
(<|>) :: Nat -> Nat -> Bool 
n <|> O = False
n <|> m = idenelse ((n <%> m) == O) True False

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n m = max n m <-> min n m

(|-|) = absDiff

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n <*> fact n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = so

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined


--abbs
o    = O
so   = S o
sso  = S so
ssso = S sso
sssso = S ssso
ssssso = S sssso
sssssso = S ssssso
ssssssso = S sssssso
sssssssso = S ssssssso
ssssssssso = S sssssssso
sssssssssso = S ssssssssso
ssssssssssso = S sssssssssso
sssssssssssso = S ssssssssssso
ssssssssssssso = S sssssssssssso
sssssssssssssso = S ssssssssssssso
ssssssssssssssso = S sssssssssssssso
sssssssssssssssso = S ssssssssssssssso
ssssssssssssssssso = S sssssssssssssssso
sssssssssssssssssso = S ssssssssssssssssso
ssssssssssssssssssso = S sssssssssssssssssso
sssssssssssssssssssso = S ssssssssssssssssssso
ssssssssssssssssssssso = S sssssssssssssssssssso
sssssssssssssssssssssso = S ssssssssssssssssssssso
ssssssssssssssssssssssso = S sssssssssssssssssssssso
sssssssssssssssssssssssso = S ssssssssssssssssssssssso
ssssssssssssssssssssssssso = S sssssssssssssssssssssssso
sssssssssssssssssssssssssso = S ssssssssssssssssssssssssso
ssssssssssssssssssssssssssso = S sssssssssssssssssssssssssso
sssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssssssssso
ssssssssssssssssssssssssssssssssssssssssssssso = S ssssssssssssssssssssssssssssssssssssssssssssso
sssssssssssssssssssssssssssssssssssssssssssssso = S sssssssssssssssssssssssssssssssssssssssssssssso