module End.Init where

import Graphics.UI.SDL
import End.Collection
import End.Collection.Header
import End.Constant.Settings
import End.Sprite.Player as Player
import End.Sprite.Fate as Fate
import End.Sprite.Fireball
import End.Util.Animation
import End.Area.Tile

import qualified Graphics.UI.SDL.Time as SdlTime

loadImageSprite :: Sprite -> Surface
loadImageSprite p = (p^.animation.image)

myPlayer :: Word32 -> Player
myPlayer d = Player 1 "Kevin" 30 25
             (Rect 100 100 100 100) (Pos 20 20)
             (Vel 0 0) 200 (Camera 10 10) DUp
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
    z <- Player.pSprite
    p <- fireballSprite
    m <- readFile "images/map" >>= return . splitMap mapWidth . read
    tiles <- loadImage "images/tileset.png" (Just bgGreen)
    return $ Gameconfig screenRes s
        [(PlayerT, z), (FireballT, p)] tiles m
        (filterRects $ getRectsFromID $
         combine 'Y' $ combine 'X' $ makeCollisionRects $ mapToColMap m)
