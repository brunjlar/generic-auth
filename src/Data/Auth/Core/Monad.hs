module Data.Auth.Core.Monad
    ( AuthM
    , auth
    , unauth
    , MonadProver (..)
    , prover
    , verifier
    , PureProver
    , runPureProver
    , PureVerifier
    , runPureVerifier
    ) where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Data.Auth.Core.Auth
import Data.Auth.Core.Authenticatable
import Data.ByteString.Builder        (Builder, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy           (ByteString)
import Data.Monoid                    (mempty, (<>))

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

class (MonadError () m, MonadReader ByteString m) => MonadVerifier m where

proverF :: MonadProver m => AuthF (m b) -> m b
proverF (Auth a cont)   = cont (authP a)
proverF (Unauth x cont) = do
    let (a, bs) = unauthP x
    writeProofStream bs
    cont a

prover :: MonadProver m => AuthM a -> m a
prover (AuthM m) = iterM proverF m

verifierF :: MonadVerifier m => AuthF (m b) -> m b
verifierF (Auth a cont)   = cont (authV a)
verifierF (Unauth x cont) = do
    bs <- ask
    case unauthV x bs of
        Nothing       -> throwError ()
        Just (a, bs') -> local (const bs') $ cont a

verifier :: MonadVerifier m => AuthM b -> m b
verifier (AuthM m) = iterM verifierF m

newtype PureProver a = PureProver (State Builder a)
    deriving (Functor, Applicative, Monad)

instance MonadProver PureProver where

    writeProofStream bs = PureProver $ modify (<> lazyByteString bs)

runPureProver :: PureProver a -> (a, ByteString)
runPureProver (PureProver m) =
    let (a, b) = runState m mempty
    in  (a, toLazyByteString b)

newtype PureVerifier a = PureVerifier (ReaderT ByteString (Either ()) a)
    deriving (Functor, Applicative, Monad, MonadReader ByteString, MonadError ())

instance MonadVerifier PureVerifier where

runPureVerifier :: PureVerifier a -> ByteString -> Maybe a
runPureVerifier (PureVerifier m) bs = case runReaderT m bs of
    Left () -> Nothing
    Right a -> Just a
