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
    , runAuthProverT
    , runAuthProverSimpleT
    , runAuthProverSimple
    ) where

import Control.Monad.State      (MonadTrans, StateT (..), modify)
import Control.Monad.Trans.Free
import Data.Foldable            (toList)
import Data.Functor.Identity    (Identity (..))
import Data.Sequence            (Seq ((:|>)), empty)

import Data.Auth.Hash           (Hash, hash)
import Data.AuthFix.Monad

newtype ProverT m a = ProverT {runProverT :: FreeT ((,) Hash) m a}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadFree ((,) Hash))

write :: Monad m => Hash -> ProverT m ()
write bs = liftF $ (bs, ())

runAuthProverT :: Monad m => AuthT Identity m a -> ProverT m a
runAuthProverT = foldFreeT g . runAuthT
  where
    g (A b h) = return $ h $ Identity b
    g (U fb h) = do
        let b = runIdentity fb
        write $ hash b
        return $ h b

proverToState :: Monad m => ProverT m a -> StateT (Seq Hash) m a
proverToState = foldFreeT g . runProverT
  where
    g (h, x) = modify (:|> h) >> return x

runAuthProverSimpleT :: Monad m => AuthT Identity m a -> m (a, [Hash])
runAuthProverSimpleT x = do
    (a, s) <- runStateT (proverToState $ runAuthProverT x) empty
    return (a, toList s)

runAuthProverSimple :: Auth Identity a -> (a, [Hash])
runAuthProverSimple = runIdentity . runAuthProverSimpleT
