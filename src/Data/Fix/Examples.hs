{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Fix.Examples
Description : fixpoint examples
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module introduces examples for fixpoints of functors.
-}

module Data.Fix.Examples
    ( ListF (..)
    , List
    , nil
    , cons
    , len
    , collapse
    , rep
    , fromTo
    , sumFromTo
    , safeTail
    , NatF (..)
    , Nat
    , zero
    , suc
    , intToNat
    , natToInt
    , add
    , mul
    , factorial
    , Stream
    , headS
    , tailS
    , takeS
    , byFromS
    , insertS
    ) where

import Data.Fix

data ListF a b = Nil | Cons a b
    deriving (Show, Eq, Ord, Functor)

-- | The fixpoint of @'ListF' a@ ist the type of lists with elements of type @a@.
type List a = Fix (ListF a)

nil :: List a
nil = wrap Nil

cons :: a -> List a -> List a
cons a = wrap . Cons a

-- | Computes the length of a 'List', using 'cata'.
--
-- >>> len $ cons True $ cons False nil
-- 2
len :: List a -> Int
len = cata g
  where
    g :: ListF a Int -> Int
    g Nil        = 0
    g (Cons _ l) = succ l

-- | Collapses a 'List' with elements in a 'Monoid'
-- into a single value, using the 'Monoid' operations.
--
-- >>> collapse $ cons "hello" $ cons " " $ cons "world" nil
-- "hello world"
collapse :: forall m. Monoid m => List m -> m
collapse = cata g
  where
    g :: ListF m m -> m
    g Nil        = mempty
    g (Cons m n) = m <> n

-- | Generates a 'List' by replicating the specified element the specified
-- number of times, using 'ana'.
--
-- >>> rep 3 'x'
-- Wrap (Cons 'x' (Wrap (Cons 'x' (Wrap (Cons 'x' (Wrap Nil))))))
rep :: forall a. Int -> a -> List a
rep n a = ana g n
  where
    g :: Int -> ListF a Int
    g m
        | m <= 0    = Nil
        | otherwise = Cons a (pred m)

-- | Generates a @'List' 'Int'@ beginning and ending at the specified numbers,
-- using 'ana'.
--
-- >>> fromTo 2 5
-- Wrap (Cons 2 (Wrap (Cons 3 (Wrap (Cons 4 (Wrap (Cons 5 (Wrap Nil))))))))
fromTo :: Int -> Int -> List Int
fromTo m n = ana g m
  where
    g :: Int -> ListF Int Int
    g i
        | i > n     = Nil
        | otherwise = Cons i (succ i)

-- | @'sumFromTo' m n@ sums all integers in the range from @m@ to @n@,
-- using 'hylo'.
--
-- >>> sumFromTo 1 100
-- 5050
sumFromTo :: Int -> Int -> Int
sumFromTo m = hylo g h
  where
    g :: Int -> ListF Int Int
    g n
        | n < m = Nil
        | otherwise = Cons n (pred n)

    h :: ListF Int Int -> Int
    h Nil        = 0
    h (Cons x y) = x + y

-- | Computes the tail of a non-empty list using 'para'.
--
-- >>> safeTail $ cons 'x' $ cons 'y' nil
-- Just (Wrap (Cons 'y' (Wrap Nil)))
-- >>> safeTail nil
-- Nothing
safeTail :: List a -> Maybe (List a)
safeTail = para g
  where
    g :: ListF a (Maybe (List a), List a) -> Maybe (List a)
    g Nil              = Nothing
    g (Cons _ (_, xs)) = Just xs

data NatF a = Zero | Succ a
    deriving (Show, Eq, Ord, Functor)

-- | The fixpoint of 'NatF' is the type of /Peano Naturals/.
type Nat = Fix NatF

-- | The Peano natural zero.
zero :: Nat
zero = wrap Zero

-- | Computes the successor of a Peano natural.
suc :: Nat -> Nat
suc = wrap . Succ

-- | Converts an 'Int' into a Peano natural using 'ana'.
--
-- >>> intToNat 2
-- Wrap (Succ (Wrap (Succ (Wrap Zero))))
intToNat :: Int -> Nat
intToNat = ana g
  where
    g :: Int -> NatF Int
    g n
        | n <= 0    = Zero
        | otherwise = Succ (pred n)

-- | Converts a Peano natural into an 'Int' using 'cata'.
--
-- >>> natToInt $ suc $ suc $ suc zero
-- 3
natToInt :: Nat -> Int
natToInt = cata g
  where
    g :: NatF Int -> Int
    g Zero     = 0
    g (Succ n) = succ n

-- | Addition of Peano naturals using 'cata'.
--
-- >>> natToInt $ add (intToNat 36) (intToNat 64)
-- 100
add :: Nat -> Nat -> Nat
add = cata g
  where
    g :: NatF (Nat -> Nat) -> Nat -> Nat
    g Zero     n = n
    g (Succ f) n = suc $ f n
--
-- | Multiplication of Peano naturals using 'cata'.
--
-- >>> natToInt $ mul (intToNat 3) (intToNat 111)
-- 333
mul :: Nat -> Nat -> Nat
mul = cata g
  where
    g :: NatF (Nat -> Nat) -> Nat -> Nat
    g Zero     _ = zero
    g (Succ f) n = add n $ f n

-- | The factorial of Peano naturals using 'para'.
--
-- >>> natToInt $ factorial $ intToNat 5
-- 120
factorial :: Nat -> Nat
factorial = para g
  where
    g :: NatF (Nat, Nat) -> Nat
    g Zero          = suc zero
    g (Succ (x, n)) = mul x $ suc n

-- | The fixpoint of functor @(a,)@ defines (infinite) streams of @a@'s.
type Stream a = Fix ((,) a)

-- | The head of a stream (computed using 'cata').
headS :: Stream a -> a
headS = cata fst

-- | The tail of a stream (computed using 'para').
tailS :: Stream a -> Stream a
tailS = para $ \(_, (_, s)) -> s

takeS :: Int -> Stream a -> [a]
takeS n s
    | n <= 0    = []
    | otherwise = headS s : takeS (pred n) (tailS s)

-- | Generates an infinite stream, using the first argument as increment
-- and the second argument as head (computed using 'ana').
--
-- >>> takeS 10 $ byFromS 2 1 :: [Int]
-- [1,3,5,7,9,11,13,15,17,19]
byFromS :: Num a => a -> a -> Stream a
byFromS d = ana $ \a -> (a, a + d)

-- | Inserts an element into a (sorted) stream (using 'apo').
--
-- >>> takeS 10 $ insertS 10 $ byFromS 2 1 :: [Int]
-- [1,3,5,7,9,10,11,13,15,17]
insertS :: forall a. Ord a => a -> Stream a -> Stream a
insertS a = apo g
  where
    g :: Stream a -> (a, Either (Stream a) (Stream a))
    g s = case headS s of
        b
            | a <= b    -> (a, Right s)
            | otherwise -> (b, Left (tailS s))
