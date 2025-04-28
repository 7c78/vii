module System.File where

import System.Environment (lookupEnv)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Maybe.X (orElseM)

readFileIfExist :: FilePath -> IO (Maybe T.Text)
readFileIfExist path = do
    exist <- Dir.doesFileExist path
    if exist then Just <$> TIO.readFile path
             else pure Nothing

-- Get $XDG_STATE_HOME or default to ~/.local/state
-- https://specifications.freedesktop.org/basedir-spec/latest/#variables
getHomeState :: IO FilePath
getHomeState =
    lookupEnv "XDG_STATE_HOME" `orElseM` do
        home <- Dir.getHomeDirectory
        pure $ home </> ".local/state"

getHomeConfig :: IO FilePath
getHomeConfig =
    lookupEnv "XDG_CONFIG_HOME" `orElseM` do
        home <- Dir.getHomeDirectory
        pure $ home </> ".config"
