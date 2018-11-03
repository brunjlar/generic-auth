{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.AuthFix.Verifier
Description : authenitcation monad verifier interpretation
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines an interpretation of the abstract authentication monad for verifiers.
-}

module Data.AuthFix.Verifier
    (
    ) where

import Control.Monad.Except     (MonadError (..))
import Control.Monad.Reader     (MonadReader (..))
import Control.Monad.Trans.Free (iterT)
import Data.Binary              (encode)
import Data.ByteString.Builder  (Builder, lazyByteString)
import Data.Functor.Identity    (Identity (..))

import Data.Auth.Hash           (hash)
import Data.AuthFix.Monad
