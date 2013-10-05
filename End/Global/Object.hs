module End.Global.Object where

import End.Collection
import End.Collection.Header
import End.Constant.Settings
import End.Function.Object
import Graphics.UI.SDL
import End.Area.Tile

fpsToInt :: (Integral a, Num b) => a -> b
fpsToInt f = fromIntegral (1000 `div` f)

nextFrame :: Object o SpriteStatus => AAction -> o -> o
nextFrame sa o
    | lfrmG > fpsToInt (sa^.fps) =
        if   frmG > sa^.maxFrames - 2
        then o & frmS .~ 0 & lfrmS .~ 0
        else o & frmS +~ 1 & lfrmS .~ 0
    | otherwise = o
  where frmG  = o^.spriteStatus.frame
        frmS  = spriteStatus.frame
        lfrmG = o^.spriteStatus.lastFrame
        lfrmS = spriteStatus.lastFrame

manageFrames :: Object o SpriteStatus => o -> GameState o
manageFrames o = do
    t <- use newTick
    Just sa <- getSpriteAction o
    return $ manageFrames' t sa o

manageFrames' :: Object o SpriteStatus => Word32 -> AAction -> o -> o
manageFrames' t sa o = nextFrame sa $ o&spriteStatus.lastFrame +~ t

objectFunction :: (Object a SpriteStatus) => Word32 -> a -> a
objectFunction b a = (a^.function) b a

fI :: Integral a => a -> Float
fI = fromIntegral

intersects :: AAction -> [Rect] -> Bool
intersects _ [] = False
intersects sp (Rect bx by bw bh:xs)
    | t py + ph > by
   && t py      < by + bh
   && t px + pw > bx
   && t px      < bx + bw = True
    | otherwise = intersects sp xs
  where t = truncate
        (px,py,pw,ph) = (sp^.box.x, sp^.box.y, sp^.box.w, sp^.box.h)

moveUp :: Integral a => Float -> Getting a s a -> s -> a -> Float
moveUp x' f sp sc
    | x' < 0    = 0
    | x' + fI (sp^.f) > fI sc = fI $ sc - sp^.f
    | otherwise = x'

manageMove :: Object o SpriteStatus => o -> GameState Pos
manageMove o = do
    t       <- use newTick
    Just sp <- getSpriteAction o
    c       <- view colls >>= return . filterNearbyTiles o sp
    return $ move sp t c o

moveFPS :: Float -> Float -> Word32 -> Float
moveFPS a b f = a + b * (fromIntegral f / 1000)

move :: Object o SpriteStatus => AAction -> Word32 -> [Rect] -> o -> Pos
move sp t c o = do
    let xx = moveFPS (o^.pos.x) (o^.vel.x) t
        yy = moveFPS (o^.pos.y) (o^.vel.y) t

    Pos (if intersects (sp&box.x +~ xx & box.y +~ o^.pos.y) c then o^.pos.x
            else (moveUp xx w sp (tileBit*mapWidth)))
        (if intersects (sp&box.y +~ yy & box.x +~ o^.pos.x) c then o^.pos.y
            else (moveUp yy  h sp (tileBit*mapHeight)))

getWalk :: Vel -> ActionTag
getWalk v | v^.x == 0 && v^.y == 0 = Stand
          | otherwise = Walk

getDir :: Direction -> Vel -> Direction
getDir d v | v^.y < 0  = DUp
           | v^.y > 0  = DDown
           | v^.x > 0  = DRight
           | v^.x < 0  = DLeft
           | otherwise = d

manageCamera :: Player -> GameState Camera
manageCamera p = do
    t <- use newTick
    return $ moveCamera t p

moveCamera :: Word32 -> Player -> Camera
moveCamera deltaTicks p =
    Camera (calcCam cxD px screenWidth mapWidthPx)
        (calcCam cyD py screenHeight mapHeightPx)
  where (Pos px py) = p^.pos
        (Camera cx cy) = p^.camera
        (Vel vx vy) = p^.vel
        delta = (fI deltaTicks) / 1000.0
        cxD  = cx  + (vx * delta)
        cyD  = cy  + (vy * delta)
        calcCam fc fp s mapPx
          | fc < 0 = 0
          | fp < fI  halfScreen = 0
          | fp > fI (mapPx - halfScreen) = fI $ mapPx - s
          | otherwise = fp - fI halfScreen
          where halfScreen = (s `div` 2)
