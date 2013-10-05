module End.Sprite.Fate where

import End.Header.Animation
import Graphics.UI.SDL
import End.Util.Animation

pSprite :: IO Sprite
pSprite = do
    s <- loadImage "images/Fate0.png" $ Just (0xee,0xff,0xde)
    return $ Sprite (pAnimation s)

pAnimation :: Surface -> Animation
pAnimation s = Animation s [oStand, oWalk] bgGreen

oStand :: (ActionTag, AAction)
oStand = (Stand, AAction 1 0 32 32 10
                      (CollisionBox 0 0 32 32))

oWalk :: (ActionTag, AAction)
oWalk = (Walk, AAction 3 0 32 32 5
                    (CollisionBox 0 0 32 32))
