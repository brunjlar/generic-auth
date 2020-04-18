{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Auth.Internal.Monad
Description : AuthT monad transformer
Copyright   : (c) Lars Brünjes, 2018
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines the @'AuthT'@ monad transformer and functions to
run an @'AuthT'@-computation as /prover/ or /verifier/.
-}

module Data.Auth.Monad where
{-
    ( AuthT
    , AuthM
    , MonadAuth (..)
    , MonadProver
    , prover
    , runProverT
    , runProver
    , MonadVerifier
    , verifier
    , verifier'
    , runVerifierT
    , runVerifierT'
    , runVerifier
    , runVerifier'
    ) where
-}
import           Control.Applicative
import           Control.Comonad.Trans.Identity (IdentityT)
import           Control.Monad.Catch            (MonadCatch, MonadThrow)
import           Control.Monad.Cont             (ContT, MonadCont)
import           Control.Monad.Except           (ExceptT, MonadError (..),
                                                 runExceptT)
import           Control.Monad.Identity         (Identity (..))
import           Control.Monad.Reader           (MonadIO, MonadPlus,
                                                 MonadReader (..),
                                                 MonadTrans (..), ReaderT (..))
import           Control.Monad.State            (MonadState, StateT, modify,
                                                 runStateT)
import qualified Control.Monad.State.Strict     as SS
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Iter       (IterT)
import           Control.Monad.Trans.Maybe      (MaybeT)
import           Control.Monad.Writer           (MonadWriter (..), WriterT)
import qualified Control.Monad.Writer.Strict    as WS
import           Data.Auth.Core
import           Data.Auth.Hash
import           Data.ByteString.Builder        (Builder, lazyByteString,
                                                 toLazyByteString)
import           Data.ByteString.Lazy           (ByteString)
import           Pipes                          (ListT)

data AuthF :: * -> * where
    Auth   :: Binary a => a -> (Auth a -> b) -> AuthF b
    Unauth :: Binary a => Auth a -> (a -> b) -> AuthF b

deriving instance Functor AuthF

-- | A monad transformer adding the ability to run computations
-- over authenticated data structures,
-- which can then be interpreted on prover- and on verifier side.
newtype AuthT m a = AuthT (FreeT AuthF m a)
    deriving (Functor, Applicative, Monad, MonadTrans,
              MonadReader r, MonadError e, MonadState s, MonadWriter w,
              MonadIO, MonadCont, MonadThrow, MonadCatch,
              Alternative, MonadPlus, MonadFree AuthF)

-- | A monad for computations over authenticated data structures
-- that can be interpreted on prover- and on verifier side.
type AuthM = AuthT Identity

-- | Type class for monads allowing computations involving
-- authenticated data structures.
class Monad m => MonadAuth m where
    auth :: Binary a => a -> m (Auth a)
    default auth :: (MonadTrans t, MonadAuth n, t n ~ m, Binary a) => a -> m (Auth a)
    auth = lift . auth

    unauth :: Binary a => Auth a -> m a
    default unauth :: (MonadTrans t, MonadAuth n, t n ~ m, Binary a) => Auth a -> m a
    unauth = lift . unauth

instance Monad m => MonadAuth (AuthT m) where
    auth a = liftF $ Auth a id
    unauth x = liftF $ Unauth x id

instance MonadAuth m => MonadAuth (IdentityT m)
instance MonadAuth m => MonadAuth (ReaderT r m)
instance MonadAuth m => MonadAuth (StateT s m)
instance MonadAuth m => MonadAuth (SS.StateT s m)
instance MonadAuth m => MonadAuth (ListT m)
instance MonadAuth m => MonadAuth (ExceptT e m)
instance MonadAuth m => MonadAuth (MaybeT m)
instance MonadAuth m => MonadAuth (ContT r m)
instance MonadAuth m => MonadAuth (IterT m)
instance (Monoid w, MonadAuth m) => MonadAuth (WriterT w m)
instance (Monoid w, MonadAuth m) => MonadAuth (WS.WriterT w m)

-- | Type class for monads suitable for the interpretation of
-- @'AuthT'@-computations on /prover/ side.
class Monad m => MonadProver m where
    writeProof :: ByteString -> m ()
    default writeProof :: (MonadTrans t, MonadProver n, t n ~ m) => ByteString -> m ()
    writeProof = lift . writeProof

instance MonadProver m => MonadProver (IdentityT m)
instance MonadProver m => MonadProver (ReaderT r m)
instance MonadProver m => MonadProver (StateT s m)
instance MonadProver m => MonadProver (SS.StateT s m)
instance MonadProver m => MonadProver (ListT m)
instance MonadProver m => MonadProver (ExceptT e m)
instance MonadProver m => MonadProver (MaybeT m)
instance MonadProver m => MonadProver (ContT r m)
instance MonadProver m => MonadProver (IterT m)
instance (Monoid w, MonadProver m) => MonadProver (WriterT w m)
instance (Monoid w, MonadProver m) => MonadProver (WS.WriterT w m)

-- | Interprets an @'AuthT'@-computation in a suitable monad
-- implementing @'MonadProver'@.
prover :: forall m t b. (Monad m, MonadTrans t, MonadProver (t m)) => AuthT m b -> t m b
prover (AuthT m) = iterTM proverF m
  where
    proverF :: AuthF (t m b) -> t m b
    proverF (Auth a c)   = c (authP a)
    proverF (Unauth x c) = do
        let (a, bs) = unauthP x
        writeProof bs
        c a

newtype ProverT m a = ProverT (StateT Builder m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => MonadProver (ProverT m) where
    writeProof bs = ProverT $ modify (<> lazyByteString bs)

-- | Interprets an @'AuthT'@-computation over an arbitrary monad
-- in prover-mode
-- by simply returning the proof-string together with the result.
runProverT :: Monad m => AuthT m a -> m (a, ByteString)
runProverT m = do
    let ProverT n = prover m
    (a, builder) <- runStateT n mempty
    return (a, toLazyByteString builder)

-- | Interprets an @'AuthM'@-computation in prover-mode
-- by simply returning the proof-string together with the result.
--
-- >>> runProver $ auth True >>= unauth
-- (True,"\SOH")
runProver :: AuthM a -> (a, ByteString)
runProver = runIdentity . runProverT

-- | Type class for monads suitable for the interpretation of
-- @'AuthT'@-computations on /verifier/ side.
type MonadVerifier m = (MonadReader ByteString m, MonadError AuthError m)

-- | Interprets an @'AuthT'@-computation in a suitable monad
-- implementing @'MonadVerifier'@.
verifier :: forall m t b. (Monad m, MonadTrans t, MonadVerifier (t m)) => AuthT m b -> t m b
verifier (AuthT m) = iterTM verifierF m
  where
    verifierF :: AuthF (t m b) -> t m b
    verifierF (Auth a c)   = c (authV a)
    verifierF (Unauth x c) = do
        bs <- ask
        case unauthV x bs of
            Left e         -> throwError e
            Right (a, bs') -> local (const bs') $ c a

-- | Interprets a function @'Auth' a -> 'AuthT' m b@ in a suitable monad
-- implementing @'MonadVerifier'@, given the hash corresponding to an @'Auth' a@ argument.
verifier' :: (Monad m, MonadTrans t, MonadVerifier (t m), Binary a)
          => (Auth a -> AuthT m b)
          -> Hash
          -> t m b
verifier' f h = verifier $ f $ AuthV h

newtype VerifierT m a = VerifierT (ReaderT ByteString (ExceptT AuthError m) a)
    deriving (Functor, Applicative, Monad, MonadReader ByteString, MonadError AuthError)

instance MonadTrans VerifierT where
    lift = VerifierT . lift . lift

-- | Interprets an @'AuthT'@-computation over an arbitrary monad
-- in verifier-mode
-- by simply taking the proof-string as an additional argument.
runVerifierT :: Monad m => AuthT m b -> ByteString -> m (Either AuthError b)
runVerifierT m bs =
    let (VerifierT n) = verifier m
    in  runExceptT $ runReaderT n bs

-- | Interprets a function @'Auth' a -> 'AuthT' m b@ in verifier-mode
-- by simply taking the proof-string as an additional argument,
-- given the hash corresponding to an @'Auth' a@ argument.
runVerifierT' :: (Binary a, Monad m)
              => (Auth a -> AuthT m b)
              -> Hash
              -> ByteString
              -> m (Either AuthError b)
runVerifierT' f h = runVerifierT (f $ AuthV h)

-- | Interprets an @'AuthM'@-computation in verifier-mode
-- by simply taking the proof-string as an additional argument.
--
-- >>> let prog = (auth 'x' >>= unauth) in let (_, bs) = runProver prog in runVerifier prog bs
-- Right 'x'
--
-- >>> let (_, bs) = runProver (auth 'x' >>= unauth) in runVerifier (auth 'y' >>= unauth) bs
-- Left AuthenticationError
runVerifier :: AuthM b -> ByteString -> Either AuthError b
runVerifier m = runIdentity . runVerifierT m

-- | Interprets a function @'Auth' a -> 'AuthM' b@ in verifier-mode
-- by simply taking the proof-string as an additional argument,
-- given the hash corresponding to an @'Auth' a@ argument.
runVerifier' :: Binary a
             => (Auth a -> AuthM b)
             -> Hash
             -> ByteString
             -> Either AuthError b
runVerifier' f h = runVerifier (f $ AuthV h)
