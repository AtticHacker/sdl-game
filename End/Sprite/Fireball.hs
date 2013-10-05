module End.Sprite.Fireball where

import End.Header.Animation
import Graphics.UI.SDL
import End.Util.Animation

fireballSprite  :: IO Sprite
fireballSprite = do
    s <- loadImage "images/Fireball.png" $ Just (0xee,0xff,0xde)
    return $ Sprite (pAnimation s)

pAnimation :: Surface -> Animation
pAnimation s = Animation s [oStand, oWalk] bgGreen

oStand :: (ActionTag, AAction)
oStand = (Stand, AAction 0 0 0 0 1
                 (CollisionBox 0 0 100 100))

oWalk :: (ActionTag, AAction)
oWalk  = (Walk, AAction 1 0 100 100 1
                (CollisionBox 0 0 100 100))
