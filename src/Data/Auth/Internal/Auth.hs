module Data.Auth.Internal.Auth
    ( Auth
    , authP
    , authV
    , unauthP
    , AuthError (..)
    , unauthV
    ) where

import Control.Exception              (Exception)
import Control.Monad.Except           (MonadError (..))
import Data.Auth.Internal.Authenticatable
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

data AuthError = DeserializationError String | AuthenticationError
    deriving Show

instance Exception AuthError

unauthV :: Authenticatable a => Auth a -> ByteString -> Either AuthError (a, ByteString)
unauthV (V h)   bs = case decodeOrFail bs of
    Left (_, _, e)    -> throwError $ DeserializationError e
    Right (bs', _, a)
        | hash a == h -> return (a, bs')
        | otherwise   -> throwError AuthenticationError
unauthV (P _ _) _  = error "illegal Auth for verifier"
