{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Internal.Auth
Description : type Auth with operations
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines type Auth and operations on it.
-}

module Data.Auth.Internal.Auth
    ( Auth
    , authP
    , authV
    , unauthP
    , AuthError (..)
    , unauthV
    ) where

import Control.Exception                  (Exception)
import Control.Monad.Except               (MonadError (..))
import Data.Auth.Internal.Authenticatable
import Data.Auth.Util.Hash
import Data.Binary                        (Binary (..), decodeOrFail, encode)
import Data.ByteString.Lazy               (ByteString)
import GHC.Generics                       (Generic)

-- | Abstract.
data Auth a =
      P Hash a -- ^ used by the /prover/
    | V Hash   -- ^ used by the /verifier/
    deriving (Show, Generic, Binary)

instance Binary a => Authenticatable (Auth a) where

    shallowCopy (P h _) = V h
    shallowCopy (V h)   = V h

-- | Used by the /prover/ to construct an @`Auth` a@.
authP :: Authenticatable a => a -> Auth a
authP a = P (hash $ shallowCopy a) a

-- | Used by the /verifier/ to construct an @`Auth` a@.
authV :: Authenticatable a => a -> Auth a
authV = V . hash

-- | Used by the /prover/ to deconstruct an @`Auth` a@.
unauthP :: Authenticatable a => Auth a -> (a, ByteString)
unauthP (P _ a) = (a, encode $ shallowCopy a)
unauthP (V _)   = error "illegal Auth for prover"

-- | Enumerates potential authentication errors
data AuthError =
      DeserializationError String -- ^ deserialization error with specified error message
    | AuthenticationError         -- ^ authentication error (hash missmatch)
    deriving Show

instance Exception AuthError

-- | Used by the /verifier/ to deconstruct an @`Auth` a@,
-- given a /certificate stream/
-- provided by the /prover/. This either succeeds with an @a@ and the rest of
-- the stream or fails with an @`AuthError`@.
unauthV :: Authenticatable a
        => Auth a     -- ^ the value to deconstruct
        -> ByteString -- ^ certificate stream provided by the prover
        -> Either AuthError (a, ByteString)
unauthV (V h)   bs = case decodeOrFail bs of
    Left (_, _, e)    -> throwError $ DeserializationError e
    Right (bs', _, a)
        | hash a == h -> return (a, bs')
        | otherwise   -> throwError AuthenticationError
unauthV (P _ _) _  = error "illegal Auth for verifier"
