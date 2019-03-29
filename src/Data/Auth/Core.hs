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
    ( Mode (..)
    , Auth (..)
    , AuthError (..)
    , authP
    , authV
    , unauthP
    , unauthV
    ) where

import Control.Exception    (Exception)
import Control.Monad.Except (MonadError (..))
import Data.ByteString.Lazy (ByteString)
import Data.Kind            (Type)

import Data.Auth.Hash
import Data.Auth.Serialize

-- | Mode of operation: 'P' for prover-mode, 'V' for verifier-mode.
data Mode = P | V deriving (Show, Eq, Ord)

-- | Authenticated version of type @a@.
-- For the /prover/, this is just an @a@,
-- but for the /verifier/, it is the `Hash` of that @a@.
data Auth :: Mode -> Type -> Type where
    AuthP :: a -> Auth 'P a
    AuthV :: Hash -> Auth 'V a

deriving instance Show a => Show (Auth m a)
deriving instance Eq a => Eq (Auth m a)
deriving instance Ord a => Ord (Auth m a)

instance Serializable a => Serializable (Auth 'P a) where
    put (AuthP a) = put $ hash a

instance Serializable (Auth 'V a) where
    put (AuthV h) = put h

instance Deserializable a => Deserializable (Auth 'P a) where
    get = AuthP <$> get

instance Deserializable (Auth 'V a) where
    get = AuthV <$> get

-- | Used by the /prover/ to construct an @`Auth` a@.
authP :: a -> Auth 'P a
authP = AuthP

-- | Used by the /verifier/ to construct an @`Auth` a@.
authV :: Serializable a => a -> Auth 'V a
authV = AuthV . hash

-- | Used by the /prover/ to deconstruct an @`Auth` a@ and a
-- /certificate stream/ for consumption by the 7verifier/.
unauthP :: Serializable a => Auth 'P a -> (a, ByteString)
unauthP (AuthP a) = (a, serialize a)

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
unauthV :: (Serializable a, Deserializable a)
        => Auth 'V a  -- ^ the value to deconstruct
        -> ByteString -- ^ certificate stream provided by the prover
        -> Either AuthError (a, ByteString)
unauthV (AuthV h) bs = case deserialize bs of
    Left e            -> throwError $ DeserializationError e
    Right (bs', a)
        | hash a == h -> return (a, bs')
        | otherwise   -> throwError AuthenticationError
