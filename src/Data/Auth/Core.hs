{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Core
Description : core functionality
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines the crucial type constructor Auth.
-}

module Data.Auth.Core
    ( Binary
    , Mode (..)
    , Auth (..)
    , AuthError (..)
    , authP
    , authV
    , unauthP
    , unauthV
    , toHash
    ) where

import Control.Exception    (Exception)
import Control.Monad.Except (MonadError (..))
import Data.Binary          (Binary (..), decodeOrFail, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Kind            (Type)

import Data.Auth.Hash

data Mode = P | V deriving (Show, Eq, Ord)

-- | Authenticated version of type @a@.
-- For the /prover/, this is just an @a@,
-- but for the /verifier/, it is the `Hash` of that @a@.
data Auth :: Mode -> Type -> Type where
    AuthP :: Binary a => a -> Auth 'P a
    AuthV :: Binary a => Hash -> Auth 'V a

deriving instance Show a => Show (Auth m a)
deriving instance Eq a => Eq (Auth m a)
deriving instance Ord a => Ord (Auth m a)

-- | Extracts the @'Hash'@ from an authenticated value.
toHash :: Auth m a -> Hash
toHash (AuthP a) = hash a
toHash (AuthV h) = h

instance Binary a => Binary (Auth 'P a) where
    put (AuthP a) = put $ hash a
    get = error "can't deserializer an Auth P"

instance Binary a => Binary (Auth 'V a) where
    put (AuthV h) = put h
    get = AuthV <$> get

-- | Used by the /prover/ to construct an @`Auth` a@.
authP :: Binary a => a -> Auth 'P a
authP = AuthP

-- | Used by the /verifier/ to construct an @`Auth` a@.
authV :: Binary a => a -> Auth 'V a
authV = AuthV . hash

-- | Used by the /prover/ to deconstruct an @`Auth` a@ and a
-- /certificate stream/ for consumption by the 7verifier/.
unauthP :: Auth 'P a -> (a, ByteString)
unauthP (AuthP a) = (a, encode a)

-- | Enumerates potential authentication errors.
data AuthError =
      DeserializationError String -- ^ deserialization error with specified error message
    | AuthenticationError         -- ^ authentication error (hash mismatch)
    deriving (Show, Read, Eq, Ord)

instance Exception AuthError

-- | Used by the /verifier/ to deconstruct an @`Auth` a@,
-- given a /certificate stream/
-- provided by the /prover/. This either succeeds with an @a@ and the rest of
-- the stream or fails with an @`AuthError`@.
unauthV :: Binary a
        => Auth 'V a  -- ^ the value to deconstruct
        -> ByteString -- ^ certificate stream provided by the prover
        -> Either AuthError (a, ByteString)
unauthV (AuthV h) bs = case decodeOrFail bs of
    Left (_, _, e)    -> throwError $ DeserializationError e
    Right (bs', _, a)
        | hash a == h -> return (a, bs')
        | otherwise   -> throwError AuthenticationError
