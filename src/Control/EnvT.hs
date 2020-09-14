{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.EnvT
  ( EnvT(..)
  , runEnvT
  , execEnvT
  , module X
  ) where

import Control.Applicative
import Control.Has
import Control.Lens
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Reader as X
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Zip
#ifndef ghcjs_HOST_OS
import Katip
import Katip.Monadic
#endif


-- | The only reason this exists is that there are a lot of instances for
-- 'EnvT, which simply pass the implementation to the inner 'm'
-- such as 'MonadLogger'.
newtype EnvT r m a = EnvT { unEnvT :: ReaderT r m a}
  deriving newtype (Functor, Applicative, Alternative, Monad
    , MonadFix, MonadFail, Contravariant, MonadZip, MonadIO, MonadPlus)

instance MonadTrans (EnvT r) where
  lift = EnvT . lift

deriving newtype instance Monad m => MonadReader r (EnvT r m)
deriving newtype instance MonadCatch m => MonadCatch (EnvT r m)
deriving newtype instance MonadThrow m => MonadThrow (EnvT r m)
deriving newtype instance MonadMask m => MonadMask (EnvT r m)
deriving newtype instance MonadError e m => MonadError e (EnvT r m)

deriving newtype instance
  ( MonadBase b m
  , Applicative b
  , Applicative m
  , Monad b
  , Monad m )
  => MonadBase b (EnvT r m)

deriving newtype instance
  (MonadBaseControl b m, Monad m)
  => MonadBaseControl b (EnvT r m)

-- | Run 'EnvT' transformer.
runEnvT :: forall r m a. EnvT r m a -> r -> m a
runEnvT m r = flip runReaderT r . unEnvT $ m
{-# INLINE runEnvT #-}

-- | Same as 'runEnvT' but with arguments flipped.
execEnvT :: forall r m a. r -> EnvT r m a -> m a
execEnvT = flip runEnvT
{-# INLINE execEnvT #-}

#ifndef ghcjs_HOST_OS
-- FIXME: These lens should probably be moved into a separate package
-- or sent to upstream katip.
makeLensesFor
  [ ("ltsLogEnv", "_ltsLogEnv")
  , ("ltsContext", "_ltsContext")
  , ("ltsNamespace", "_ltsNamespace")
  ] ''KatipContextTState

instance (Has KatipContextTState r, MonadIO m)
  => KatipContext (EnvT r m) where
  getKatipContext = view $ part . _ltsContext
  localKatipContext f = local (part . _ltsContext %~ f)
  getKatipNamespace = view $ part . _ltsNamespace
  localKatipNamespace f = local (part . _ltsNamespace %~ f)

instance (Has KatipContextTState r, MonadIO m)
  => Katip (EnvT r m) where
  getLogEnv = view $ part . _ltsLogEnv
  localLogEnv f = local (part . _ltsLogEnv %~ f)
#endif

instance MonadResource m => MonadResource (EnvT r m) where
  liftResourceT = lift . liftResourceT
