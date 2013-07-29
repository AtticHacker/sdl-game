{-# LANGUAGE TemplateHaskell #-}
module End.Util.Screen where

import End.Collection
import End.Header.Config
import Graphics.UI.SDL
import End.Header.Animation

myScreen :: GameState Surface
myScreen = do
    s <- view screen
    liftIO $ setVideoMode (s^.w) (s^.h) 32 [SWSurface]
