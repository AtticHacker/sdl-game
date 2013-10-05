module End.Game where

import End.Collection
import End.Collection.Header
import End.Collection.Util
import End.Area.Tile
import End.Global.Loop ( manageObjects, updateDelta
                       , inputAndLoop, drawBG
                       , managePlayer)

game :: GameState ()
game = do
    loop

loop :: GameState ()
loop = do
    updateDelta
    view gameScreen >>= revForM
       [ drawAllTiles
       -- , drawColls -- Draw collision boxes debug
       , managePlayer
       , manageObjects
       , inputAndLoop loop
       ]
