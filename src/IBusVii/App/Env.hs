module IBusVii.App.Env where

import Data.IORef
import Data.Maybe
import IBusVii.App.Log
import IBusVii.App.Options
import IBusVii.PreeditText as PreeditText
import Vii.InputMethod

data Env = Env
    { envDebugLogging :: Bool
    , envLogger       :: Logger
    , envInputMethod  :: InputMethod
    , envPreeditText  :: IORef PreeditText
    }

instance HasLogger Env where
    getLogger = envLogger
    {-# INLINE getLogger #-}
    getDebugLogging = envDebugLogging
    {-# INLINE getDebugLogging #-}

newEnv :: IO Env
newEnv = do
    preeditText <- newIORef PreeditText.empty
    logger <- setupLogger
    mOptions <- readConfig logger
    let options = fromMaybe defaultAppOptions mOptions
        inputMethod =
            case optActiveInputMethod options of
                "telex" -> telex
                "vni"   -> vni
                _       -> undefined
    pure Env
        { envDebugLogging = (optDebugLogging options)
        , envLogger = logger
        , envPreeditText = preeditText
        , envInputMethod = inputMethod
        }
