module End.Global.Drawing where

import End.Collection
import Graphics.UI.SDL
import End.Collection.Header
import End.Function.Object
import End.Util.State

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface xx yy src dst clip = blitSurface src clip dst offset
 where offset = Just $ Rect xx yy 0 0

drawFrame :: Object o SpriteStatus => Surface -> o -> GameState ()
drawFrame s o = do
    Just a <- getSpriteAction o
    Just v <- getSprite o
    Camera cx cy <- player ## (^.camera)
    let Pos ox oy = o^.pos
    void $ liftIO $ applySurface
        (truncate $ ox - cx) (truncate $ oy - cy)
        (v^.animation.image) s $ Just $ Rect
         (fromEnum  (a^.w) * (o^.spriteStatus.frame))
         (a^.h * (dirId $ o^.direction)) (a^.w) (a^.h)
