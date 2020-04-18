{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
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
    ) where

import Data.Functor.Compose  (Compose)
import Data.Functor.Const    (Const)
import Data.Functor.Identity (Identity)
import Data.Functor.Product  (Product)
import Data.Functor.Sum      (Sum)
import GHC.Generics

class Functor f => FunctorKleisli f where
    lambda :: Monad m => f (m a) -> m (f a)
    default lambda :: (Monad m, Generic1 f, FunctorKleisli (Rep1 f)) => f (m a) -> m (f a)
    lambda = fmap to1 . lambda . from1

instance FunctorKleisli f => FunctorKleisli (M1 i c f) where
    lambda = fmap M1 . lambda . unM1

instance FunctorKleisli (K1 i c) where
    lambda = return . K1 . unK1

instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (f :*: g) where
    lambda (fma :*: gma) = (:*:) <$> lambda fma <*> lambda gma

instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (f :+: g) where
    lambda (L1 fma) = L1 <$> lambda fma
    lambda (R1 gma) = R1 <$> lambda gma

instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (f :.: g) where
    lambda = fmap Comp1 . lambda . fmap lambda . unComp1

instance FunctorKleisli Par1 where
    lambda = fmap Par1 . unPar1

instance FunctorKleisli f => FunctorKleisli (Rec1 f) where
    lambda = fmap Rec1 . lambda . unRec1

instance FunctorKleisli Identity
instance FunctorKleisli (Const a)
instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (Product f g)
instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (Sum f g)
instance (FunctorKleisli f, FunctorKleisli g) => FunctorKleisli (Compose f g)
