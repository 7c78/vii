module IBusVii.Engine where

import GHC.Stack
import System.Environment (getExecutablePath)
import Control.Monad
import Data.Word (Word32)
import Data.Bits ((.&.))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.IORef
import DBus (ObjectPath, BusName, formatBusName)
import DBus.Client (DBusR, autoMethod)
import Vii.ViSymbol (ViSymbol)
import Vii.InputMethod qualified as Vi
import IBus.Text qualified as IBusText
import IBus.Engine qualified as E
import IBus.Modifier qualified as Modifier
import IBus.Key qualified as Key
import IBus.Common
import IBusVii.App
import IBusVii.Word32
import IBusVii.PreeditText qualified as PreeditText
import IBusVii.PreeditText (PreeditText)

viiEngineBusName :: BusName
viiEngineBusName = "org.freedesktop.IBus.vii"

viiEngineObjectPath :: ObjectPath
viiEngineObjectPath = "/org/freedesktop/IBus/Engine/vii"

installViiEngine :: IO ()
installViiEngine = do
    execPath <- T.pack <$> getExecutablePath
    let busName = T.pack $ formatBusName viiEngineBusName
        xmlComponent = T.unlines
            [ "<component>"
            , "  <name>"<>busName<>"</name>"
            , "  <exec>"<>execPath<>"</exec>"
            , "  <license>GPLv3</license>"
            , "  <homepage>https://github.com/7c78/vii#readme</homepage>"
            , ""
            , "  <engines>"
            , "    <engine>"
            , "      <name>vii</name>"
            , "      <longname>Vii</longname>"
            , "      <language>vi</language>"
            , "      <license>GPLv3</license>"
            , "    </engine>"
            , "  </engines>"
            , "</component>"
            ]
    TIO.writeFile "/usr/share/ibus/component/vii.xml" xmlComponent

viiEngineDef :: Env -> DBusR ()
viiEngineDef env = do
    E.requestBusName viiEngineBusName
    E.factoryInterface
        [ autoMethod "CreateEngine" createEngine
        ]
    E.engineInterface viiEngineObjectPath
        [ autoMethod "ProcessKeyEvent" processKeyEvent
        , autoMethod "Reset"           reset
        ]

    where
        createEngine :: String -> DBusR ObjectPath
        createEngine _name =
            pure viiEngineObjectPath

        processKeyEvent :: Word32 -> Word32 -> Word32 -> DBusR Bool
        processKeyEvent keyval _keycode modifiers
            | modifiers .&. Modifier.release_mask /= 0 =
                pure False

            | modifiers .&. Modifier.control_mask /= 0
              || modifiers .&. Modifier.alt_mask /= 0
              || modifiers .&. Modifier.super_mask /= 0
              || modifiers .&. Modifier.hyper_mask /= 0
              || modifiers .&. Modifier.meta_mask /= 0
              || modifiers .&. Modifier.ignored_mask /= 0 =
                pure False

            | keyval `elem` [Key.shift_l, Key.shift_r] =
                pure False

            | keyval == Key.backspace = do
                (len, preedit) <- runAppAsEngine env removeLastLetter
                case len of
                    -1 -> pure False
                    0  -> do commitText0
                             pure True
                    _  -> do updateText (PreeditText.render preedit)
                             pure True

            | Just symbols <- Vi.readSymbol (envInputMethod env) char = do
                preedit <- runAppAsEngine env (composeChar char symbols)
                updateText (PreeditText.render preedit)
                pure True

            | otherwise = do
                preedit <- runAppAsEngine env (  currentPreeditText
                                              <* clearPreeditText)
                commitText (PreeditText.render preedit)
                pure False

            where
                char = chr32 keyval

        reset :: DBusR ()
        reset =
            runAppAsEngine env clearPreeditText

        updateText :: Text -> DBusR ()
        updateText t =
            E.updatePreeditText viiEngineObjectPath
                (IBusText.new t)
                (textLength t)
                True
                ibus_engine_preedit_commit

        commitText :: Text -> DBusR ()
        commitText t = do
            when (T.length t > 0) $ do
                E.hidePreeditText viiEngineObjectPath
                E.commitText viiEngineObjectPath (IBusText.new t)

        commitText0 :: DBusR ()
        commitText0 = do
            E.hidePreeditText viiEngineObjectPath
            E.commitText viiEngineObjectPath (IBusText.new "")

currentPreeditText :: App PreeditText
currentPreeditText = liftIO . readIORef =<< asks envPreeditText

writePreeditText :: PreeditText -> App ()
writePreeditText t = do
    ref <- asks envPreeditText
    liftIO $ writeIORef ref t

composeChar :: (HasCallStack) => Char -> [ViSymbol] -> App PreeditText
composeChar char symbols = do
    preedit <- currentPreeditText
    let preedit' = PreeditText.composeChar char symbols preedit
    writePreeditText preedit'

    trace $ f (T.singleton char) (PreeditText.render preedit')
    pure preedit'

removeLastLetter :: (HasCallStack) => App (Int, PreeditText)
removeLastLetter = do
    preedit <- currentPreeditText

    if PreeditText.null preedit then
        pure (-1, PreeditText.empty)
    else do
        let preedit' = PreeditText.removeLastLetter preedit
        writePreeditText preedit'

        trace $ f (PreeditText.render preedit) (PreeditText.render preedit')
        pure (PreeditText.length preedit', preedit')

clearPreeditText :: (HasCallStack) => App ()
clearPreeditText = do
    preedit <- currentPreeditText
    when (PreeditText.length preedit > 0) $ do
        writePreeditText PreeditText.empty

        trace $ PreeditText.render preedit
