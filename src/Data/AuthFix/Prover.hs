{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.AuthFix.Prover
Description : authenitcation monad prover interpretation
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines an interpretation of the abstract authentication monad for provers.
-}

module Data.AuthFix.Prover
    ( ProverT (..)
    , P
    , runAuthProverT
    , runAuthProverSimpleT
    , runAuthProverSimple
    ) where

import Control.Monad.State      (MonadTrans, StateT (..), modify)
import Control.Monad.Trans.Free
import Data.Binary              (Binary (..), encode)
import Data.ByteString.Builder  (Builder, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy     (ByteString)
import Data.Functor.Identity    (Identity (..))

import Data.Auth.Hash           (hash)
import Data.AuthFix.Monad

newtype ProverT m a = ProverT {runProverT :: FreeT ((,) ByteString) m a}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadFree ((,) ByteString))

write :: Monad m => ByteString -> ProverT m ()
write bs = liftF $ (bs, ())

newtype P a = P {getP :: a}
    deriving (Show, Eq, Ord)

instance Binary a => Binary (P a) where
    put (P a) = put $ hash a
    get = error "P: get not supported"

runAuthProverT :: Monad m => AuthT P m a -> ProverT m a
runAuthProverT = foldFreeT g . runAuthT
  where
    g (A b h) = return $ h $ P b
    g (U fb h) = do
        let b = getP fb
        write $ encode b
        return $ h b

proverToState :: Monad m => ProverT m a -> StateT Builder m a
proverToState = foldFreeT g . runProverT
  where
    g (bs, x) = modify (<> lazyByteString bs) >> return x

runAuthProverSimpleT :: Monad m => AuthT P m a -> m (a, ByteString)
runAuthProverSimpleT x = do
    (a, b) <- runStateT (proverToState $ runAuthProverT x) mempty
    return (a, toLazyByteString b)

runAuthProverSimple :: Auth P a -> (a, ByteString)
runAuthProverSimple = runIdentity . runAuthProverSimpleT
