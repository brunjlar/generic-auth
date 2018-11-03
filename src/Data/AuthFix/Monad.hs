{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.AuthFix.Monad
Description : abstract authenitcation monad
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines an abstract authentication monad.
-}

module Data.AuthFix.Monad
    ( AuthF (..)
    , AuthT (..)
    , Auth
    , auth
    , unauth
    , runAuthIdentityT
    , runAuthIdentity
    ) where

import Control.Monad.Except      (MonadError)
import Control.Monad.Reader      (MonadIO, MonadReader)
import Control.Monad.State       (MonadState)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Free
import Control.Monad.Writer      (MonadWriter)
import Data.Binary               (Binary)
import Data.Functor.Identity     (Identity (..))

data AuthF f a where
    A :: Binary b => b -> (f b -> a) -> AuthF f a
    U :: Binary b => f b -> (b -> a) -> AuthF f a

instance Functor (AuthF f) where
    fmap g (A b h)  = A b (g . h)
    fmap g (U fb h) = U fb (g . h)

newtype AuthT f m a = AuthT {runAuthT :: FreeT (AuthF f) m a}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadFree (AuthF f),
              MonadWriter w, MonadReader r, MonadState s, MonadError e, MonadIO)

auth :: (Monad m, Binary b) => b -> AuthT f m (f b)
auth b = liftF $ A b id

unauth :: (Monad m, Binary b) => f b -> AuthT f m b
unauth fb = liftF $ U fb id

runAuthIdentityT :: Monad m => AuthT Identity m a -> m a
runAuthIdentityT = iterT g . runAuthT
  where
    g (A b h)  = h $ Identity b
    g (U fb h) = h $ runIdentity fb

type Auth f = AuthT f Identity

runAuthIdentity :: Auth Identity a -> a
runAuthIdentity = runIdentity . runAuthIdentityT
