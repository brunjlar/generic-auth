module Data.Auth.Core.Auth
    ( Auth
    , authP
    , authV
    , unauthP
    , unauthV
    ) where

import Data.Auth.Core.Authenticatable
import Data.Auth.Core.Role
import Data.Auth.Util.Hash
import Data.Binary                    (Binary (..))

data Auth :: Role -> * -> * where
    P :: Hash -> a -> Auth 'Prover a
    V :: Hash -> Auth 'Verifier a

deriving instance Show a => Show (Auth r a)

instance Binary (Auth 'Verifier a) where

    get = V <$> get

    put (V h) = put h

instance Authenticatable (Auth r a) where

    shallowCopy (P h _) = V h
    shallowCopy (V h)   = V h

authP :: Authenticatable a => a -> Auth 'Prover a
authP a = P (hash $ shallowCopy a) a

authV :: Authenticatable a => Shallow a -> Auth 'Verifier a
authV = V . hash

unauthP :: Authenticatable a => Auth 'Prover a -> (a, Shallow a)
unauthP (P _ a) = (a, shallowCopy a)

unauthV :: Authenticatable a => Auth 'Verifier a -> Shallow a -> Maybe (Shallow a)
unauthV (V h) a = if h == hash a then Just a
                                 else Nothing
