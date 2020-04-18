{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.AuthFix.Fix
Description : fixpoints of functors in a Kleisli category
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines fixpoints of functors in a Kleisli category and associated functions.
-}

module Data.AuthFix.Fix
    ( FixA
    , wrapA
    , unwrapA
    , cataA
    , anaA
    ) where

import Data.AuthFix.Monad
import Data.Binary (Binary)
import GHC.Generics (Generic)

newtype FixA f g = FixA (f (g (FixA f g)))
    deriving Generic

deriving instance (forall a. Show a => Show (f (g a))) => Show (FixA f g)
deriving instance (forall a. Eq a => Eq (f (g a))) => Eq (FixA f g)
deriving instance (Eq (FixA f g), forall a. Ord a => Ord (f (g a))) => Ord (FixA f g)

instance ( forall a. Binary a => Binary (g a)
         , forall a. Binary a => Binary (f a)
         ) => Binary (FixA f g) where

wrapA :: f (g (FixA f g)) -> FixA f g
wrapA = FixA

unwrapA :: FixA f g -> f (g (FixA f g))
unwrapA (FixA x) = x

cataA :: ( forall b. Binary b => Binary (g b)
         , forall b. Binary b => Binary (f b)
         , Traversable g
         , Monad m
         )
      => (g a -> a)
      -> FixA f g
      -> AuthT f m a
cataA h x = do
    gy <- unauth $ unwrapA x
    ga <- sequence $ fmap (cataA h) gy
    return $ h ga

anaA :: ( forall b. Binary b => Binary (g b)
        , forall b. Binary b => Binary (f b)
        , Traversable g
        , Monad m
        )
     => (a -> g a)
     -> a
     -> AuthT f m (FixA f g)
anaA h a = do
    let ga = h a
    gx  <- sequence $ fmap (anaA h) ga
    fgx <- auth gx
    return $ wrapA fgx
