module Main where

import Control.Monad
import Options.Applicative
import IBus.Engine
import IBusVii.Engine
import IBusVii.App

main :: IO ()
main = join $ execParser cmdParserInfo

cmdParserInfo :: ParserInfo (IO ())
cmdParserInfo = info (cmdParser <**> helper)
    (  fullDesc
    <> header "Vii - a Vietnamese input method engine"
    )

cmdParser :: Parser (IO ())
cmdParser =
        hsubparser
            (  command "install" (info (pure installViiEngine) (progDesc "Install the engine"))
            <> command "config" (info (pure generateConfigFile) (progDesc "Generate a config file"))
            )
    <|> pure engine

engine :: IO ()
engine = do
    env <- newEnv
    runEngine $ viiEngineDef env
