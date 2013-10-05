module End.Global.Loop where

import qualified Graphics.UI.SDL.Time as SdlTime
import End.Collection
import End.Collection.Util
import End.Collection.Header
import End.UI
import Graphics.UI.SDL as SDL hiding (name)
import End.Function.Object
import End.Global.Object
import End.Global.Drawing

managePlayer :: Surface -> GameState ()
managePlayer s = do
    player ### manageMove   >>= (player.pos .=)
    player ### manageCamera >>= (player.camera .=)
    k <- player ### manageFrames
    drawFrame s k
    player .= k
    zoom player $ do
        d <- use direction
        vel ## getDir d >>= (direction .=)
        vel ## getWalk  >>= (spriteStatus.actionTag .=)

manageObjects :: Surface -> GameState ()
manageObjects s = void $ manageObjects' temps temps s

manageObjects' objT1 objT2 s = do
    t <- use newTick
    objects.objT2.traverse %= (objectFunction t)
    objects.objT1 ### mapM manageFrames >>= (objects.objT2 .=)
    objects.objT1 ##  isExpired         >>= setS (objects.objT2)
    void $ objects.objT1 ## map (drawFrame s) >>= sequence

updateDelta :: GameState ()
updateDelta = do
    d <- liftIO SdlTime.getTicks
    oldTick ## (d-) >>= (newTick .=)
    oldTick .= d

inputAndLoop :: GameState () -> Surface -> GameState ()
inputAndLoop loop s = do
    liftIO $ SDL.flip s
    whileEvents handleInput >>= \e -> unless e loop

drawBG :: Surface -> GameState ()
drawBG gamescreen = liftIO $ do
    jrect <- Just `liftM` getClipRect gamescreen
    color <- mapRGB' gamescreen 0xff 0xff 0xff
    void $ fillRect gamescreen jrect color
