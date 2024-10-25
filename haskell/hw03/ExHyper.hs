module ExHyper where

import Prelude hiding ( exp )

-- Nat datatype --------------------------------

data Nat = O | S Nat
     deriving (Eq, Show)

instance (Num Nat) where
    (+) = add
    (*) = mul
    abs = id
    fromInteger 0 = O
    fromInteger n
      | n > 0     = S $ fromInteger (n-1)
      | otherwise = O
    signum O = O
    signum n = S O
    negate n = O

instance (Ord Nat) where
    O     <= m     = True
    (S n) <= O     = False
    (S n) <= (S m) = n <= m

add :: Nat -> Nat -> Nat
add n O     = n
add n (S m) = S (add m n)

mul :: Nat -> Nat -> Nat
mul n O     = O
mul n (S m) = add (mul n m) n

exp :: Nat -> Nat -> Nat
exp n O     = S O
exp n (S m) = mul (exp n m) n

hyper :: Integral i => i -> (Nat -> Nat -> Nat)
hyper 0 = add 
hyper 1 = mul
hyper 2 = exp 
hyper _ = error "unexpected i"

--abbs
o    = O
so   = S o
sso  = S so
ssso = S sso
sssso = S ssso
ssssso = S sssso