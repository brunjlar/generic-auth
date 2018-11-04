{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK show-extensions     #-}

{-|
Module      : Data.AuthFix.Example
Description : examples
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module contains some examples.
-}

module Data.AuthFix.Example
    (
    ) where

import Control.Monad.Except
import Data.AuthFix.Monad
import Data.AuthFix.Prover
import Data.AuthFix.Verifier
import Data.Binary           (Binary)
import Data.Functor.Const (Const (..))
import GHC.Generics          (Generic)

newtype Tree f a = Tree (f (Tree' f a))
    deriving Generic

data Tree' f a =
      Leaf a
    | Node (Tree f a) (Tree f a)
    deriving Generic

deriving instance (Show a, forall x. Show x => Show (f x)) => Show (Tree f a)
deriving instance (Show a, Show (Tree f a)) => Show (Tree' f a)

instance (Binary a, forall x. Binary x => Binary (f x)) => Binary (Tree f a)
instance (Binary a, Binary (Tree f a)) => Binary (Tree' f a)

deriving instance Binary a => Binary (Const a x)

leaf :: (Binary a, forall x. Binary x => Binary (f x), Monad m)
     => a
     -> AuthT f m (Tree f a)
leaf = fmap Tree . auth . Leaf

node :: (Binary a, forall x. Binary x => Binary (f x), Monad m)
     => Tree f a
     -> Tree f a
     -> AuthT f m (Tree f a)
node l r = Tree <$> auth (Node l r)

leftmost :: (Binary a, forall x. Binary x => Binary (f x), Monad m)
         => Tree f a
         -> AuthT f m a
leftmost (Tree ft) = do
    t <- unauth ft
    case t of
        Leaf a   -> return a
        Node l _ -> leftmost l

sampleTree :: (forall x. Binary x => Binary (f x), Monad m) => AuthT f m (Tree f Char)
sampleTree = do
    l <- leaf 'x'
    r <- leaf 'y'
    node l r

test :: Either VerificationError Char
test = do
    let (tp, bs) = runAuthProverSimple sampleTree
    tv <- runAuthVerifierSimple sampleTree bs
    let (_, cs) = runAuthProverSimple $ leftmost tp
    runAuthVerifierSimple (leftmost tv) cs
