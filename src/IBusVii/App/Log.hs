module IBusVii.App.Log where

import GHC.Stack (HasCallStack, withFrozenCallStack, callStack)
import System.Directory qualified as Dir
import System.File qualified as Dir
import System.FilePath ((</>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Exception (Exception(..))
import Data.Text (Text)
import Data.Text qualified as T
import Colog (LogAction, Severity(..), Msg(..), (<&), showSourceLoc, withLogTextFile)

type Logger = (LogAction IO Text -> IO ()) -> IO ()

class HasLogger env where
    getLogger       :: env -> Logger
    getDebugLogging :: env -> Bool

type WithLog env m = (HasCallStack, HasLogger env, MonadReader env m, MonadIO m)

showSeverity :: Severity -> Text
showSeverity = \case
    Debug   -> "[Debug]  "
    Info    -> "[Info]   "
    Warning -> "[Warning]"
    Error   -> "[Error]  "

fmtMsg :: Msg Severity -> Text
fmtMsg (Msg ser stack text) =
    T.concat [ showSeverity ser
             , showSourceLoc stack
             , " "
             , text
             ]

emitLog :: (HasCallStack) => Logger -> Severity -> Text -> IO ()
emitLog logger severity message =
    logger $ \log ->
        withFrozenCallStack $ log <& fmtMsg (Msg severity callStack message)

logMessage :: forall env m. (WithLog env m) => Severity -> Text -> m ()
logMessage severity message = do
    debug <- asks getDebugLogging
    when debug $ do
        logger <- asks getLogger
        liftIO $ withFrozenCallStack $ emitLog logger severity message

logInfo :: forall env m. (WithLog env m) => Text -> m ()
logInfo = withFrozenCallStack (logMessage Info)

logWarning :: forall env m. (WithLog env m) => Text -> m ()
logWarning = withFrozenCallStack (logMessage Warning)

logError :: forall env m. (WithLog env m) => Text -> m ()
logError = withFrozenCallStack (logMessage Error)

logException :: forall e env m. (WithLog env m, Exception e) => e -> m ()
logException = withFrozenCallStack (logError . T.pack . displayException)

trace :: forall env m. (WithLog env m) => Text -> m ()
trace = withFrozenCallStack (logMessage Debug)

f :: Text -> Text -> Text
f i o = T.concat [i, " -> ", o]

-- setup

setupLogger :: IO Logger
setupLogger = do
    (statePath, logPath) <- getLogPath
    Dir.createDirectoryIfMissing False statePath
    pure $ withLogTextFile logPath

getLogPath :: IO (FilePath, FilePath)
getLogPath = do
    homeState <- Dir.getHomeState
    let logDir = homeState </> "vii"
        logFile = logDir </> "debug.log"
    pure (logDir, logFile)
