module Data.Auth.Core.Authenticatable
    ( Authenticatable (..)
    ) where

import Data.Auth.Util.Hash
import Data.Binary         (Binary)

class Binary a => Authenticatable a where

    shallowCopy :: a -> a

instance Authenticatable Char where

    shallowCopy = id

instance Authenticatable Hash where

    shallowCopy = id

instance Authenticatable a => Authenticatable [a] where

    shallowCopy = map shallowCopy

instance (Authenticatable a, Authenticatable b) => Authenticatable (a, b) where

    shallowCopy (a, b) = (shallowCopy a, shallowCopy b)

instance (Authenticatable a, Authenticatable b) => Authenticatable (Either a b) where

    shallowCopy = either (Left . shallowCopy) (Right . shallowCopy)
