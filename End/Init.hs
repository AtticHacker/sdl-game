module End.Init where

import Graphics.UI.SDL
import End.Util.Animation
import End.Collection
import End.Collection.Header
import End.Constant.Settings
import End.Sprite.Player

import qualified Graphics.UI.SDL.Time as SdlTime

loadImagePlayer :: Player -> IO Surface
loadImagePlayer p = loadImage (p^.sprite.animation.image) $ Just (0xee,0xff,0xde)

myPlayer :: IO Player
myPlayer = do
    let widt  = 162
        heigh = 150

    return $ Player 1 "Kevin" 30 25 widt heigh
        (Rect 100 100 100 100)
        (Pos 20 20) (Vel 0 0) (Camera 10 10)
        pSprite dead undefined

dead :: (Object a e) => a -> Bool
dead = (>= 0) . (^.pos.x)

screenRes :: Screen
screenRes = Screen screenWidth screenHeight

initState :: IO Gamestate
initState = do
    p <- myPlayer
    initDelta <- SdlTime.getTicks
    return $ (Gamestate p) (Objectlist []) initDelta

initConfig :: IO Gameconfig
initConfig = do
    s <- setVideoMode screenWidth screenHeight 32 [SWSurface]
    return $ Gameconfig screenRes s
