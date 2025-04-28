module IBus.Engine where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent (threadDelay)
import Data.Word (Word32)
import Data.Text (Text)
import DBus (ObjectPath, InterfaceName, BusName, Variant, IsVariant(..), signal, signalBody)
import DBus.Client (DBusR, Method, Interface(..), requestName, defaultInterface, connect, emit, export)
import IBus.Address (getIBusAddress)
import IBus.Text (IBusText)

ibusFactoryPath :: ObjectPath
ibusFactoryPath = "/org/freedesktop/IBus/Factory"

ibusFactoryInterface :: InterfaceName
ibusFactoryInterface = "org.freedesktop.IBus.Factory"

ibusEngineInterface :: InterfaceName
ibusEngineInterface = "org.freedesktop.IBus.Engine"

runEngine :: DBusR () -> IO ()
runEngine action = do
    client <- connect =<< getIBusAddress
    runReaderT action client
    forever $ threadDelay maxBound

requestBusName :: BusName -> DBusR ()
requestBusName busName = do
    client <- ask
    liftIO $ void $ requestName client busName []

factoryInterface :: [Method] -> DBusR ()
factoryInterface methods = do
    client <- ask
    liftIO $ export client ibusFactoryPath $ defaultInterface
        { interfaceName = ibusFactoryInterface
        , interfaceMethods = methods
        }

engineInterface :: ObjectPath -> [Method] -> DBusR ()
engineInterface enginePath methods = do
    client <- ask
    liftIO $ export client enginePath $ defaultInterface
        { interfaceName = ibusEngineInterface
        , interfaceMethods = methods
        }

updatePreeditText :: ObjectPath -> IBusText -> Word32 -> Bool -> Word32 -> DBusR ()
updatePreeditText enginePath text cursorPos visible mode = do
    client <- ask
    liftIO $ emit client
        (signal enginePath ibusEngineInterface "UpdatePreeditText")
            { signalBody =
                [ v text
                , u cursorPos
                , b visible
                , u mode
                ]
            }

commitText :: ObjectPath -> IBusText -> DBusR ()
commitText enginePath text = do
    client <- ask
    liftIO $ emit client
        (signal enginePath ibusEngineInterface "CommitText")
            { signalBody =
                [ v text
                ]
            }

hidePreeditText :: ObjectPath -> DBusR ()
hidePreeditText enginePath = do
    client <- ask
    liftIO $ emit client
        (signal enginePath ibusEngineInterface "HidePreeditText")

showPreeditText :: ObjectPath -> DBusR ()
showPreeditText enginePath = do
    client <- ask
    liftIO $ emit client
        (signal enginePath ibusEngineInterface "ShowPreeditText")

-- | variant
v :: forall a. (IsVariant a) => a -> Variant
v = toVariant . toVariant
{-# INLINE v #-}

-- | string
s :: Text -> Variant
s = toVariant
{-# INLINE s #-}

-- | uint32
u :: Word32 -> Variant
u = toVariant
{-# INLINE u #-}

-- | boolean
b :: Bool -> Variant
b = toVariant
{-# INLINE b #-}
