module Data.Auth.Core.Auth
    ( Auth
    , authP
    , authV
    , unauthP
    , unauthV
    ) where

import Control.Monad                  (guard, MonadPlus (..))
import Data.Auth.Core.Authenticatable
import Data.Auth.Util.Hash
import Data.Binary                    (Binary (..), encode, decodeOrFail)
import Data.ByteString.Lazy           (ByteString)
import GHC.Generics                   (Generic)

data Auth a = P Hash a | V Hash
    deriving (Show, Generic)

instance Binary a => Binary (Auth a) where

instance Binary a => Authenticatable (Auth a) where

    shallowCopy (P h _) = V h
    shallowCopy (V h)   = V h

authP :: Authenticatable a => a -> Auth a
authP a = P (hash $ shallowCopy a) a

authV :: Authenticatable a => a -> Auth a
authV = V . hash

unauthP :: Authenticatable a => Auth a -> (a, ByteString)
unauthP (P _ a) = (a, encode $ shallowCopy a)
unauthP (V _)   = error "illegal Auth for prover"

unauthV :: Authenticatable a => Auth a -> ByteString -> Maybe (a, ByteString)
unauthV (V h)   bs = case decodeOrFail bs of
    Left _            -> mzero
    Right (bs', _, a) -> do
        guard $ hash a == h
        return (a, bs')
unauthV (P _ _) _  = error "illegal Auth for verifier"
