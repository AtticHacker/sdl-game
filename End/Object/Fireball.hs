module End.Object.Fireball where

import End.Collection
import End.Header.Animation
import End.Header.Object
import End.Header.Object.Temp
import End.Collection.Header
import End.Function.Object
import End.Collection.Util

fireBall :: GameState Temp
fireBall = do
    objs      <- use $ objects.temps
    genId     <- liftIO $ generateID (objs^..traverse.iden)
    Just v    <- player ### getSpriteAction
    Pos px py <- use $ player.pos
    newT      <- use newTick
    let fI = fromIntegral
        offScreen = (> 400 + px) . (^.pos.x)
    return $ Temp genId "FireBall" 1 goRightObject
        (SpriteStatus FireballT Walk 0 newT)
        (Pos (px + (fI $ v^.w)) (py + 10)) (Vel 0 0) offScreen DUp

goRightObject a o = o & pos.x +~ (300 * (fromIntegral a / 1000.0))
