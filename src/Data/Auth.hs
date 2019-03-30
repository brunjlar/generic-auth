{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth
Description : authenticated data structures
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module reexports all definitions
necessary to work with authenticated data structures.
-}

module Data.Auth
    ( ByteString
    , Serializable
    , Deserializable
    , Generic
    , Mode (..)
    , Auth
    , AuthError (..)
    , serialize
    , deserialize
    , toProver
    , toVerifier
    , toVerifier'
    , module Data.Auth.Hash
    , module Data.Auth.Monad
    ) where

import Data.ByteString.Lazy (ByteString)
import GHC.Generics

import Data.Auth.Core
import Data.Auth.Hash
import Data.Auth.Monad
import Data.Auth.Serialize

-- | Extracts the value from a monadic prover-mode computation.
toProver :: AuthM 'P a -> a
toProver = fst . runProver

-- | Extracts the value on verifier-side from a monadic prover-mode computation.
toVerifier :: ( Serializable (f 'P)
              , Deserializable (f 'V)
              )
           => AuthM 'P (f 'P)
           -> f 'V
toVerifier = unsafeDeserialize . serialize . toProver

-- | Version of 'toVerifier' for 'Auth' values.
toVerifier' :: ( Serializable (f 'P)
              , Deserializable (f 'V)
              )
            => AuthM 'P (Auth (f 'P) 'P)
            -> Auth (f 'V) 'V
toVerifier' = unsafeDeserialize . serialize . toProver
