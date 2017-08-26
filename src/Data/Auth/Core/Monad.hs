module Data.Auth.Core.Monad
    ( AuthM
    , auth
    , unauth
    , MonadProver (..)
    , prover
    , verifier
    ) where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.State
import Data.Auth.Core.Auth
import Data.Auth.Core.Authenticatable
import Data.ByteString.Lazy           (ByteString)

data AuthF :: * -> * where
    Auth   :: Authenticatable a => a -> (Auth a -> b) -> AuthF b
    Unauth :: Authenticatable a => Auth a -> (a -> b) -> AuthF b

deriving instance Functor AuthF

newtype AuthM a = AuthM (Free AuthF a)
    deriving (Functor, Applicative, Monad)

auth :: Authenticatable a => a -> AuthM (Auth a)
auth a = AuthM $ liftF $ Auth a id

unauth :: Authenticatable a => Auth a -> AuthM a
unauth x = AuthM $ liftF $ Unauth x id

class Monad m => MonadProver m where

    writeProofStream :: ByteString -> m ()

proverF :: MonadProver m => AuthF (m b) -> m b
proverF (Auth a cont)   = cont (authP a)
proverF (Unauth x cont) = do
    let (a, bs) = unauthP x
    writeProofStream bs
    cont a

prover :: MonadProver m => AuthM a -> m a
prover (AuthM m) = iterM proverF m

verifierF :: (MonadError () m, MonadState ByteString m) => AuthF (m b) -> m b
verifierF (Auth a cont)   = cont (authV a)
verifierF (Unauth x cont) = do
    bs <- get
    case unauthV x bs of
        Nothing       -> throwError ()
        Just (a, bs') -> do
            put bs'
            cont a

verifier :: (MonadError () m, MonadState ByteString m) => AuthM b -> m b
verifier (AuthM m) = iterM verifierF m
