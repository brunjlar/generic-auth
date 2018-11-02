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
    ) where

import Data.Fix

data ListF a b = Nil | Cons a b
    deriving (Show, Eq, Ord, Functor)

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
