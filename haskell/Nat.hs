module Nat where

import qualified Prelude as P hiding (Num(..))

data Nat = O | S Nat
    deriving (P.Eq, P.Show)

--soma 
plus :: (Nat, Nat) -> Nat
plus (n, O) = n
plus (n, S m) = S (plus (n, m)) 
--soma currificada
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S (n <+> m)

--subtracao
monus :: (Nat, Nat) -> Nat
monus (n, O) = n
monus (O, _) = O
monus (S n, S m) = monus (n, m)
--subtracao currificada 
(<->) :: Nat -> Nat -> Nat
n <-> O = n
O <-> _ = O
S n <-> S m = n <-> m

--multiplicacao
times :: (Nat, Nat) -> Nat
times (n, O) = O
times (n, S m) = plus (n, times (n, m)) 
--multiplicacao currificado
(<*>) :: Nat -> Nat -> Nat 
n <*> O = O
n <*> S m = n <+> (n Nat.<*> m)

--exponencial
expo :: (Nat, Nat) -> Nat
expo (_, O) = S O
expo (O, _) = O
expo (n, S m) = times (n, expo (n, m))
--exponencial currificado
(<^>) :: Nat -> Nat -> Nat
_ <^> O = S O
O <^> _ = O
n <^> (S m) = n Nat.<*> (n <^> m)


--first 
fst :: (Nat, Nat) -> Nat
fst (n, m) = n
--second
snd :: (Nat, Nat) -> Nat
snd (n, m) = m

--divisao
div :: (Nat, Nat) -> (Nat, Nat)
div (_, O) = P.error "não divide por 0"
div (O, _) = (O, O)
div (n, S O) = (n, O)




--predecessor
p :: Nat -> Nat
p O = O
p (S n) = n

--dobro
double :: Nat -> Nat
double n = times(n, sso)

--fatorial
fact :: Nat -> Nat
fact O = S O
fact (S n) = times(S n, fact n)

--fibonacci
fib :: Nat -> Nat
fib (S (S n)) = fib (S n) <+> fib n
fib _ = so

--maximo
max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, n) = n
max (S n, S m) = S (Nat.max (n, m))
--minimo
min :: (Nat, Nat) -> Nat
min (_, O) = O
min (O, _) = O
min (S n, S m) = S (min (n, m))


--abreviação 
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