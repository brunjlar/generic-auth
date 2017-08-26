{-# LANGUAGE TypeOperators #-}

module Data.Auth.Internal.Authenticatable
    ( Authenticatable (..)
    ) where

import Data.Auth.Util.Hash
import Data.Binary         (Binary)
import GHC.Generics

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
