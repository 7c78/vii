module IBus.Address where

import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.File (getHomeConfig, readFileIfExist)
import Control.Exception (Exception, throw)
import Control.Applicative
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Maybe.X (orElseM, altM)
import DBus (Address, parseAddress)

data IBusAddressException
    = IBusAddressMissingException
    | IBusAddressInvalidException
    deriving (Show)
    deriving anyclass (Exception)

getLocalMachineId :: IO String
getLocalMachineId = do
    mMachineId <-     readFileIfExist "/var/lib/dbus/machine-id"
               `altM` readFileIfExist "/etc/machine-id"
    case mMachineId of
        Nothing  -> error "Unable to load /var/lib/dbus/machine-id"
        Just mId -> pure $ T.unpack $ T.stripEnd mId

getSocketPath :: IO FilePath
getSocketPath =
    lookupEnv "IBUS_ADDRESS_FILE" `orElseM` do
        configDir <- getHomeConfig
        display <- lookupEnv "WAYLAND_DISPLAY" `orElseM` error "DE is not supported"
        machineId <- getLocalMachineId
        pure $ configDir </> "ibus/bus" </> (machineId <> "-unix-" <> display)

getIBusAddressString :: IO String
getIBusAddressString =
    lookupEnv "IBUS_ADDRESS" `orElseM` do
        socketPath <- getSocketPath
        contents <- TIO.readFile socketPath
        let addr = asum $ map (T.stripPrefix "IBUS_ADDRESS=") (T.lines contents)
        case addr of
            Nothing   -> throw IBusAddressMissingException
            Just addr -> pure $ T.unpack addr

getIBusAddress :: IO Address
getIBusAddress = do
    s <- getIBusAddressString
    case parseAddress s of
        Nothing   -> throw IBusAddressInvalidException
        Just addr -> pure addr
