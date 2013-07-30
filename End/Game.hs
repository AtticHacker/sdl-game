module End.Game where

import End.Collection
import End.Collection.Util
import End.Collection.Header
import End.Collection.Object (move, drawObjects, drawObject)
import End.Global.Loop       (setNewTick, updateDelta, drawBG, inputAndLoop)
import Graphics.UI.SDL
import Graphics.UI.SDL.Time as SdlTime

game :: GameState ()
game = do
    setNewTick
    gamescreen <- view gameScreen
    move

    updateDelta

    drawBG      gamescreen
    use player >>= drawObject gamescreen
    drawObjects gamescreen


    inputAndLoop game gamescreen
