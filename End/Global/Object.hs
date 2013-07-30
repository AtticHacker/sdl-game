module End.Global.Object where

import End.Collection
import Graphics.UI.SDL
import End.Collection.Header
import End.Constant.Settings
import qualified Graphics.UI.SDL.Time as SdlTime
import End.Function.Object


getSprite :: (Object t SpriteStatus) => t -> GameState (Maybe Sprite)
getSprite b = view images >>= return . lookup (b^.spriteStatus.tag)

getSpriteAction :: (Object t SpriteStatus) => t -> GameState (Maybe AAction)
getSpriteAction b = do
    Just sp <- getSprite b
    return $ lookup (b^.spriteStatus.actionTag) (sp^.animation.actions)

fpsToInt :: (Integral a, Num b) => a -> b
fpsToInt f = fromIntegral (1000 `div` f)

nextFrame :: Object s SpriteStatus => s -> GameState ()
nextFrame o = do
    Just z <- o&getSpriteAction
    if o^.spriteStatus.lastFrame > fpsToInt (z^.fps)
        then if o^.spriteStatus.frame > z^.maxFrames - 2
             then player.spriteStatus.frame .= 0
                  >> player.spriteStatus.lastFrame .= 0
             else player.spriteStatus.frame += 1
                  >> player.spriteStatus.lastFrame .= 0
        else return ()

drawObject :: (Object a SpriteStatus) => Surface -> a -> GameState Bool
drawObject s o = do
    use newTick >>= (player.spriteStatus.lastFrame +=)
    nextFrame o
    drawFrame s o

drawFrame :: (Object a SpriteStatus) => Surface -> a -> GameState Bool
drawFrame s o = do
    Just v <- getSprite o
    Just a <- getSpriteAction o

    liftIO $ blitSurface (v^.animation.image)
        (Just $ Rect
         (fromEnum  (a^.w) * (o^.spriteStatus.frame))
         (a^.h * (dirId $ o^.direction))
         (a^.w)
         (a^.h)) s offset
  where offset = Just $ Rect (fromEnum (o^.pos.x)) (fromEnum (o^.pos.y)) 0 0

drawObjects :: Surface -> GameState [Bool]
drawObjects s = use newTick >>= manageObjects temps temps s

manageObjects :: Object a SpriteStatus =>
                 (([a] -> Accessor [a] [a]) -> Objectlist
                  -> Accessor [a] Objectlist) ->
                 (([a] -> Mutator [a]) -> Objectlist ->
                  Mutator Objectlist) -> Surface ->
                 Word32 -> GameState [Bool]

manageObjects objT1 objT2 s d = do
    objects.objT2.traverse %= (objectFunction d)
    objs <- use $ objects.objT1
    objects.objT2 .= isExpired objs
    sequence $ map (drawObject s) objs

getNewTick :: GameState Word32
getNewTick = do
    d <- liftIO SdlTime.getTicks
    use oldTick >>= \dl -> return $ d -dl

objectFunction :: (Object a SpriteStatus) => Word32 -> a -> a
objectFunction b a = (a^.function) b a

move :: GameState ()
move = do
    cPlayer <- use player
    deltaTicks <- use newTick
    let x'  = cPlayer^.pos.x + (cPlayer^.vel.x * (fromIntegral deltaTicks / 1000.0))
        y'  = cPlayer^.pos.y + (cPlayer^.vel.y * (fromIntegral deltaTicks / 1000.0))
        x'' = if x' < 0 then 0 else
                if (x' + fromIntegral 162) > fromIntegral screenWidth then fromIntegral $ screenWidth - 162 else x'
        y'' = if y' < 0 then 0 else
                if (y' + fromIntegral 150) > fromIntegral screenHeight then fromIntegral $ screenHeight - 150 else y'
    player.pos .= Pos x'' y''
