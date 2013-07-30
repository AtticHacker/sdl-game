module End.Global.Loop where

import qualified Graphics.UI.SDL.Time as SdlTime
import End.Collection
import End.Collection.Header
import End.UI
import Graphics.UI.SDL as SDL hiding (name)
import End.Collection.Util

setNewTick :: GameState ()
setNewTick = do
    d <- liftIO SdlTime.getTicks
    use oldTick >>= \dl -> newTick .= d -dl

updateDelta :: GameState ()
updateDelta = liftIO SdlTime.getTicks >>= (oldTick .=)

inputAndLoop :: GameState () -> Surface -> GameState ()
inputAndLoop loop s = do
    liftIO $ SDL.flip s
    whileEvents handleInput >>= \e -> unless e loop

drawBG :: Surface -> GameState Bool
drawBG gamescreen = liftIO $ do
    jrect <- Just `liftM` getClipRect gamescreen
    color <- mapRGB' gamescreen 0xff 0xff 0xff
    fillRect gamescreen jrect color
