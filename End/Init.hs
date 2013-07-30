module End.Init where

import Graphics.UI.SDL
import End.Util.Animation
import End.Collection
import End.Collection.Header
import End.Constant.Settings
import End.Sprite.Player

import qualified Graphics.UI.SDL.Time as SdlTime

loadImageSprite :: Sprite -> Surface
loadImageSprite p = (p^.animation.image)

myPlayer :: Word32 -> Player
myPlayer d = Player 1 "Kevin" 30 25
             (Rect 100 100 100 100) (Pos 20 20)
             (Vel 0 0) 100 (Camera 10 10) DUp
             (SpriteStatus PlayerT Walk 2 d)
             dead undefined

dead :: (Object a e) => a -> Bool
dead = (>= 0) . (^.pos.x)

screenRes :: Screen
screenRes = Screen screenWidth screenHeight

initState :: IO Gamestate
initState = do
    initDelta <- SdlTime.getTicks
    return $ Gamestate (myPlayer initDelta) (Objectlist []) initDelta initDelta

initConfig :: IO Gameconfig
initConfig = do
    s <- setVideoMode screenWidth screenHeight 32 [SWSurface]
    z <- pSprite
    return $ Gameconfig screenRes s [(PlayerT, z)]
