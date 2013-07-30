module End.Sprite.Player where

import End.Header.Animation
import Graphics.UI.SDL
import End.Util.Animation

pSprite :: IO Sprite
pSprite = do
    s <- loadImage "images/60x110.png" $ Just (0xee,0xff,0xde)
    return $ Sprite (pAnimation s) (Rect 0 0 0 0) (Rect 0 0 0 0)

pAnimation :: Surface -> Animation
pAnimation s = Animation s [playerStand, playerWalk] bgGreen

playerStand :: (ActionTag, AAction)
playerStand  = (Stand, AAction 1 0 60 110 1)

playerWalk :: (ActionTag, AAction)
playerWalk   = (Walk, AAction 8 0 60 110 10)