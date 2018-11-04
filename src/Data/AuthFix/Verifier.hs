{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    ( VerifierF (..)
    , VerifierT (..)
    , V
    , verify
    , VerificationError (..)
    , runAuthVerifierT
    , runAuthVerifierSimpleT
    , runAuthVerifierSimple
    ) where

import           Control.Monad.Except     (ExceptT (..), MonadError (..),
                                           MonadTrans (..), runExceptT)
import           Control.Monad.State      (MonadState, StateT, evalStateT)
import qualified Control.Monad.State      as S
import           Control.Monad.Trans.Free (FreeT, MonadFree, foldFreeT, liftF)
import           Data.Binary              (Binary (..), decodeOrFail, encode)
import           Data.ByteString.Lazy     (ByteString)
import           Data.Functor.Identity    (Identity (..))

import           Data.Auth.Hash           (Hash, hash)
import           Data.AuthFix.Monad

data VerifierF a where
    Verify :: Binary b => Hash -> (b -> a) -> VerifierF a

instance Functor VerifierF where
    fmap g (Verify h f) = Verify h (g . f)

newtype VerifierT m a = VerifierT {runVerifierT :: FreeT VerifierF m a}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadFree VerifierF)

verify :: (Monad m, Binary b) => Hash -> VerifierT m b
verify h = liftF $ Verify h id

newtype V a = V {getV :: Hash}
    deriving (Show, Eq, Ord)

instance Binary (V a) where
    put (V a) = put a
    get = V <$> get

runAuthVerifierT :: Monad m => AuthT V m a -> VerifierT m a
runAuthVerifierT = foldFreeT g . runAuthT
  where
    g (A b f) = return $ f $ V $ hash b
    g (U h f) = do
        b <- verify $ getV h
        return $ f b

data VerificationError =
      DeserializationError String
    | AuthenticationError ByteString Hash
    deriving (Show, Eq, Ord)

newtype ES m a = ES {runES :: StateT ByteString (ExceptT VerificationError m) a}
    deriving (Functor, Applicative, Monad,
              MonadState ByteString, MonadError VerificationError)

instance MonadTrans ES where
    lift = ES . lift . lift

runER' :: Monad m => ES m a -> ByteString -> m (Either VerificationError a)
runER' x = runExceptT . evalStateT (runES x)

verifierToES :: Monad m => VerifierT m a -> ES m a
verifierToES = foldFreeT g . runVerifierT
  where
    g (Verify h f) = do
        bs <- S.get
        case decodeOrFail bs of
            Left (_, _, e)    -> throwError $ DeserializationError e
            Right (bs', _, b)
                | hash b /= h -> throwError $ AuthenticationError (encode b) h
                | otherwise   -> S.put bs' >> return (f b)

runAuthVerifierSimpleT :: Monad m
                       => AuthT V m a
                       -> ByteString
                       -> m (Either VerificationError a)
runAuthVerifierSimpleT = runER' . verifierToES . runAuthVerifierT

runAuthVerifierSimple :: Auth V a
                      -> ByteString
                      -> Either VerificationError a
runAuthVerifierSimple x = runIdentity . runAuthVerifierSimpleT x
