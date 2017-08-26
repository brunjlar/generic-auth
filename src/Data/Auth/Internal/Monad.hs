module Data.Auth.Internal.Monad
    ( AuthT
    , AuthM
    , MonadAuth (..)
    , MonadProver
    , prover
    , runProverT
    , runProver
    , MonadVerifier
    , verifier
    , runVerifierT
    , runVerifier
    ) where

import           Control.Applicative
import           Control.Comonad.Trans.Identity     (IdentityT)
import           Control.Monad.Catch                (MonadCatch, MonadThrow)
import           Control.Monad.Cont                 (MonadCont, ContT)
import           Control.Monad.Except               (MonadError (..), ExceptT, runExceptT)
import           Control.Monad.Identity             (Identity (..))
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Iter           (IterT)
import           Control.Monad.Trans.Maybe          (MaybeT)
import           Control.Monad.Reader               (MonadReader (..), ReaderT (..), MonadTrans (..), MonadIO, MonadPlus)
import           Control.Monad.State                (MonadState, StateT, modify, runStateT)
import qualified Control.Monad.State.Strict         as SS
import           Control.Monad.Writer               (MonadWriter (..), WriterT)
import qualified Control.Monad.Writer.Strict        as WS
import           Data.Auth.Internal.Auth
import           Data.Auth.Internal.Authenticatable
import           Data.ByteString.Builder            (Builder, lazyByteString, toLazyByteString)
import           Data.ByteString.Lazy               (ByteString)
import           Data.Monoid                        (mempty, (<>))
import           Pipes                              (ListT)

data AuthF :: * -> * where
    Auth   :: Authenticatable a => a -> (Auth a -> b) -> AuthF b
    Unauth :: Authenticatable a => Auth a -> (a -> b) -> AuthF b

deriving instance Functor AuthF

newtype AuthT m a = AuthT (FreeT AuthF m a)
    deriving (Functor, Applicative, Monad, MonadTrans,
              MonadReader r, MonadError e, MonadState s, MonadWriter w,
              MonadIO, MonadCont, MonadThrow, MonadCatch,
              Alternative, MonadPlus, MonadFree AuthF)

type AuthM = AuthT Identity

class Monad m => MonadAuth m where
    auth :: Authenticatable a => a -> m (Auth a)
    default auth :: (MonadTrans t, MonadAuth n, t n ~ m, Authenticatable a) => a -> m (Auth a)
    auth = lift . auth

    unauth :: Authenticatable a => Auth a -> m a
    default unauth :: (MonadTrans t, MonadAuth n, t n ~ m, Authenticatable a) => Auth a -> m a
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

runProverT :: Monad m => AuthT m a -> m (a, ByteString)
runProverT m = do
    let ProverT n = prover m
    (a, builder) <- runStateT n mempty
    return (a, toLazyByteString builder)

runProver :: AuthM a -> (a, ByteString)
runProver = runIdentity . runProverT

type MonadVerifier m = (MonadReader ByteString m, MonadError AuthError m)

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

newtype VerifierT m a = VerifierT (ReaderT ByteString (ExceptT AuthError m) a)
    deriving (Functor, Applicative, Monad, MonadReader ByteString, MonadError AuthError)

instance MonadTrans VerifierT where
    lift = VerifierT . lift . lift

runVerifierT :: Monad m => AuthT m a -> ByteString -> m (Either AuthError a)
runVerifierT m bs =
    let (VerifierT n) = verifier m
    in  runExceptT $ runReaderT n bs

runVerifier :: AuthM a -> ByteString -> Either AuthError a
runVerifier m = runIdentity . runVerifierT m
