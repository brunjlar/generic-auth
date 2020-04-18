{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.AuthFix.Fix.Example
Description : examples
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module contains some examples.
-}

module Data.AuthFix.Fix.Example
    (
    ) where

import Data.Binary                  (Binary)
import Data.List.NonEmpty           (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.AuthFix.Fix
import Data.AuthFix.Kleisli
import Data.AuthFix.Monad
import Data.AuthFix.Prover          hiding (P)
import Data.AuthFix.Verifier

type TreeF a = K a :+: (I :*: I)

type Tree f a = FixA f (TreeF a)

fromNonEmpty :: ( Binary a
                , Monad m
                , forall b. Binary b => Binary (f b)
                )
             => NonEmpty a
             -> AuthT f m (Tree f a)
fromNonEmpty = anaA g
  where
    g (a :| [])       = Inl (K a)
    g (a :| (b : cs)) = let (ds, es) = split cs
                        in  Inr $ I (a :| ds) :*: I (b :| es)

split :: [a] -> ([a], [a])
split []           = ([], [])
split [a]          = ([a], [])
split (a : b : cs) = let (ds, es) = split cs in (a : ds, b : es)

rightmost :: ( Binary a
             , Monad m
             , forall b. Binary b => Binary (f b)
             )
          => Tree f a
          -> AuthT f m a
rightmost = cataA g
  where
    g (Inl (K a))       = a
    g (Inr (_ :*: I a)) = a

sampleTree :: (forall b. Binary b => Binary (f b)) => Auth f (Tree f Char)
sampleTree = fromNonEmpty $ 'a' :| "bcdefghij"

test :: Either VerificationError Char
test = do
    let (tp, bs) = runAuthProverSimple sampleTree
    tv <- runAuthVerifierSimple sampleTree bs
    let (_, cs) = runAuthProverSimple $ rightmost tp
    runAuthVerifierSimple (rightmost tv) cs
