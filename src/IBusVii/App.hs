module IBusVii.App
    ( module X
    , App(..)
    , runApp
    , runAppAsEngine
    ) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad.Reader as X
import Control.Monad.Catch as X (MonadThrow(..))
import DBus.Client (DBusR)
import IBusVii.App.Log as X
import IBusVii.App.Options as X
import IBusVii.App.Env as X

newtype App a = App
    { unApp :: ReaderT Env IO a
    }
    deriving newtype
        ( Functor, Applicative, Monad
        , MonadIO
        , MonadReader Env
        )

instance MonadThrow App where
    throwM = liftIO . throwIO
    {-# INLINE throwM #-}

runApp :: forall a. Env -> App a -> IO a
runApp = flip (runReaderT . unApp)

runAppAsEngine :: forall a. Env -> App a -> DBusR a
runAppAsEngine env action =
    liftIO $ runApp env $ do
        result <- liftIO $ try @SomeException (runApp env action)
        case result of
            Left e  -> logException e *> throwM e
            Right x -> pure x
