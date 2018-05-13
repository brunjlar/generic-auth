{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Internal.Authenticatable
Description : class Authenticatable and instances
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines class Authenticatable and its instances.
-}

module Data.Auth.Internal.Authenticatable
    ( Authenticatable (..)
    ) where

import Data.Auth.Util.Hash
import Data.Binary         (Binary)
import GHC.Generics

-- | A type is @`Authenticatable`@ if it admits making a /shallow copy/.
-- For primitive types, @`shallowCopy`@ is simply the identity.
-- For compound types, @`shallowCopy`@ is invoked recursively on all
-- constituents.
-- Instances can be derived generically.
class Binary a => Authenticatable a where
    shallowCopy :: a -> a
    default shallowCopy :: (Generic a, Authenticatable' (Rep a)) => a -> a
    shallowCopy = to . shallowCopy' . from

instance Authenticatable Bool where
    shallowCopy = id

instance Authenticatable Char where
    shallowCopy = id

instance Authenticatable Int where
    shallowCopy = id

instance Authenticatable Float where
    shallowCopy = id

instance Authenticatable Double where
    shallowCopy = id

instance Authenticatable Hash where
    shallowCopy = id

instance Authenticatable a => Authenticatable [a] where
    shallowCopy = map shallowCopy

instance (Authenticatable a, Authenticatable b) => Authenticatable (a, b) where
    shallowCopy (a, b) = (shallowCopy a, shallowCopy b)

instance (Authenticatable a, Authenticatable b) => Authenticatable (Either a b) where
    shallowCopy = either (Left . shallowCopy) (Right . shallowCopy)

class Authenticatable' f where
    shallowCopy' :: f p -> f p

instance Authenticatable' V1 where
    shallowCopy' _ = error "shallowCopy of empty type"

instance Authenticatable' U1 where
    shallowCopy' U1 = U1

instance (Authenticatable' f, Authenticatable' g) => Authenticatable' (f :+: g) where
    shallowCopy' (L1 x) = L1 $ shallowCopy' x
    shallowCopy' (R1 x) = R1 $ shallowCopy' x

instance (Authenticatable' f, Authenticatable' g) => Authenticatable' (f :*: g) where
    shallowCopy' (f :*: g) = shallowCopy' f :*: shallowCopy' g

instance Authenticatable c => Authenticatable' (K1 i c) where
    shallowCopy' (K1 x) = K1 (shallowCopy x)

instance Authenticatable' f => Authenticatable' (M1 i t f) where
    shallowCopy' (M1 x) = M1 (shallowCopy' x)
