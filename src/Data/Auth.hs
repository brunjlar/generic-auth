module Data.Auth
    ( Auth
    , Authenticatable (..)
    , authP
    , authV
    , unauthP
    , unauthV
    ) where

import Data.Auth.Hash
import Data.Binary    (Binary)
import GHC.Generics   (Generic)

data Auth a =
      Prover Hash a
    | Verifier Hash
    deriving (Generic, Show)

instance Binary a => Binary (Auth a) where

class Binary a => Authenticatable a where

    shallowCopy :: a -> a
    shallowCopy = id

instance Authenticatable Char where

instance Authenticatable Hash where

instance Authenticatable a => Authenticatable [a] where

    shallowCopy = map shallowCopy

instance (Authenticatable a, Authenticatable b) => Authenticatable (a, b) where

    shallowCopy (a, b) = (shallowCopy a, shallowCopy b)

instance (Authenticatable a, Authenticatable b) => Authenticatable (Either a b) where

    shallowCopy = either (Left . shallowCopy) (Right . shallowCopy)

instance Binary a => Authenticatable (Auth a) where

    shallowCopy (Prover h _)   = Verifier h
    shallowCopy v@(Verifier _) = v

hashAuth :: Authenticatable a => a -> Hash
hashAuth = hash . shallowCopy

authP :: Authenticatable a => a -> Auth a
authP a = Prover (hashAuth a) a

authV ::  Authenticatable a => a -> Auth a
authV = Verifier . hashAuth

unauthP :: Authenticatable a => Auth a -> a
unauthP (Prover _ a) = a
unauthP (Verifier _) = error "expected Prover"

unauthV :: Authenticatable a => Auth a -> a -> a
unauthV (Prover _ _) _ = error "expected Verifier"
unauthV (Verifier h) a = if h == hash a then a
                                        else error "verification error"
