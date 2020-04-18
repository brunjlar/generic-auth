{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.AuthFix.Kleisli
Description : functors that can be lifted into Kleisli categories
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines functors that can be lifted into Kleisli categories.
-}

module Data.AuthFix.Kleisli
    ( FunctorKleisli (..)
    , I (..)
    , K (..)
    , (:*:) (..)
    , (:+:) (..)
    , C (..)
    ) where

import Data.Binary  (Binary)
import GHC.Generics (Generic)

class Functor f => FunctorKleisli f where
    lambda :: Monad m => f (m a) -> m (f a)

newtype I a = I a
    deriving (Show, Read, Eq, Ord, Binary, Generic, Functor)

instance FunctorKleisli I where
    lambda (I ma) = fmap I ma

newtype K a b = K a
    deriving (Show, Read, Eq, Ord, Binary, Generic, Functor)

instance FunctorKleisli (K a) where
    lambda (K a) = return $ K a

data (f :*: g) a = f a :*: g a
    deriving (Show, Read, Eq, Ord, Generic, Functor)

instance (Binary (f a), Binary (g a)) => Binary ((f :*: g) a)

instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (f :*: g) where
    lambda (fma :*: gma) = (:*:) <$> lambda fma <*> lambda gma

data (f :+: g) a = Inl (f a) | Inr (g a)
    deriving (Show, Read, Eq, Ord, Generic, Functor)

instance (Binary (f a), Binary (g a)) => Binary ((f :+: g) a)

instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (f :+: g) where
    lambda (Inl fma) = Inl <$> lambda fma
    lambda (Inr gma) = Inr <$> lambda gma

newtype C f g a = C (f (g a))
    deriving (Show, Read, Eq, Ord, Generic, Binary, Functor)

instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (C f g) where
    lambda (C fgma) = C <$> lambda (lambda <$> fgma)
