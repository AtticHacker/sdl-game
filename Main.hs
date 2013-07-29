module Main where

import Graphics.UI.SDL   (withInit, InitFlag(InitEverything))
import End.Collection    (evalStateT, runReaderT)
import End.Init          (initConfig, initState)
import End.Header.Config (Gameconfig, Gamestate)
import End.Game          (game)

main :: IO ()
main = withInit [InitEverything] $ do
    pureConfig <- initConfig
    pureState  <- initState
    startState pureConfig pureState

startState :: Gameconfig -> Gamestate -> IO ()
startState = evalStateT . runReaderT game
