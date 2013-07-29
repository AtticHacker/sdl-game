module End.Game where

import End.Collection        (view)
import End.Collection.Header (GameState, gameScreen)
import End.Collection.Object (move, drawPlayer, drawObjects)
import End.Global.Loop       (getNewTick, updateDelta, drawBG, inputAndLoop)

game :: GameState ()
game = do
    newTick <- getNewTick
    gamescreen <- view gameScreen
    move newTick
    updateDelta

    drawBG      gamescreen
    drawPlayer  gamescreen
    drawObjects gamescreen newTick

    inputAndLoop game gamescreen
