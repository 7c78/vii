module IBusVii.App.Options where

import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.File qualified as Dir (getHomeState)
import Control.Monad
import Control.Exception (displayException)
import Data.Yaml (ToJSON(..), FromJSON(..), object, withObject, encodeFile, decodeFileEither, (.=), (.:))
import Data.Text qualified as T
import Colog qualified
import IBusVii.App.Log

data AppOptions = AppOptions
    { optDebugLogging :: Bool
    , optActiveInputMethod  :: String
    }

defaultAppOptions :: AppOptions
defaultAppOptions = AppOptions
    { optDebugLogging = False
    , optActiveInputMethod = "telex"
    }

instance ToJSON AppOptions where
    toJSON (AppOptions debugLogging inputMethod) =
        object [ "debug-logging" .= debugLogging
               , "active-input-method" .= inputMethod
               ]

instance FromJSON AppOptions where
    parseJSON = withObject "AppOptions" $ \v ->
        pure AppOptions
        <*> v .: "debug-logging"
        <*> v .: "active-input-method"

-- read

readConfig :: Logger -> IO (Maybe AppOptions)
readConfig logger = do
    (_, configFile) <- getConfigPath
    configExists <- Dir.doesFileExist configFile
    if configExists then do
        result <- decodeFileEither @AppOptions configFile
        case result of
            Left err -> do
                emitLog logger Colog.Error (T.pack $ displayException err)
                pure Nothing
            Right val ->
                pure (Just val)
    else
        pure Nothing

-- write

generateConfigFile :: IO ()
generateConfigFile = do
    (configDir, configFile) <- getConfigPath
    configExists <- Dir.doesFileExist configFile
    unless configExists $ do
        Dir.createDirectoryIfMissing False configDir
        encodeFile configFile defaultAppOptions

getConfigPath :: IO (FilePath, FilePath)
getConfigPath = do
    homeState <- Dir.getHomeState
    let configDir = homeState </> "vii"
        configFile = configDir </> "config.yaml"
    pure (configDir, configFile)
