module End.Global.Object where

import End.Collection
import Graphics.UI.SDL
import End.Collection.Util
import End.Collection.Header
import Data.Word
import End.Constant.Settings
import qualified Graphics.UI.SDL.Time as SdlTime

runOnSelf :: (a -> a -> b) -> a -> b
runOnSelf f a = (f a) a

generateID :: [Int] -> IO Int
generateID xs = do
    rand <- randomInt 1 99999
    return $ generateID' rand xs

generateID' :: Int -> [Int] -> Int
generateID' _ [] = 1
generateID' i xs
    | i `elem` xs = generateID' (i+1) xs
    | otherwise   = i

removeObj :: (Eq a, HasIden a1 a, HasIden s a) => s -> [a1] -> [a1]
removeObj _ [] = []
removeObj a (x':xs)
    | a^.iden /= x'^.iden = x' : removeObj a xs
    | otherwise           = foldr (\o acc -> o : acc)
                            xs $ removeObj a []

addObject :: (MonadState s m, HasObjects s e) =>
             (([a] -> Mutator [a]) -> e -> Mutator e) -> a -> m ()
addObject s o = objects.s %= (o:)

loadImagePlayer p = loadImage (p^.sprite.animation.image) $ Just (0xee,0xff,0xde)

drawObject :: (Object s Sprite) => Surface -> s -> GameState Bool
drawObject s p = do
    a <- liftIO $ loadImagePlayer p
    liftIO $ blitSurface a (Just $ Rect 10 10 50 100) s offset
  where offset = Just $ Rect (fromEnum (p^.pos.x)) (fromEnum (p^.pos.y)) 0 0


drawObjects :: Surface -> Word32 -> GameState [Bool]
drawObjects s = manageObjects temps temps s

manageObjects :: Object a Sprite =>
                 (([a] -> Accessor [a] [a]) -> Objectlist
                  -> Accessor [a] Objectlist) ->
                 (([a] -> Mutator [a]) -> Objectlist ->
                  Mutator Objectlist) -> Surface ->
                 Word32 ->
                 ReaderT Gameconfig (StateT Gamestate IO) [Bool]
manageObjects objT1 objT2 s d = do
    objects.objT2.traverse %= (objectFunction d)
    objs <- use $ objects.objT1
    objects.objT2 .= isExpired objs
    sequence $ map (drawObject s) objs

getNewTick :: GameState Word32
getNewTick = do
    d <- liftIO SdlTime.getTicks
    use delta >>= \dl -> return $ d -dl


isExpired :: (Object a b) => [a] -> [a]
isExpired [] = []
isExpired (xx:xs)
    | (xx^.expire) xx = isExpired xs
    | otherwise       = xx : isExpired xs


-- objectFunction :: (Object a e) => a -> a
-- objectFunction = runOnSelf $ (^.function)
objectFunction :: (Object a e) => Word32 -> a -> a
objectFunction w o = (o^.function) w o

drawPlayer :: Surface -> GameState Bool
drawPlayer s = use player >>= drawObject s

move :: Word32 -> GameState ()
move deltaTicks = do
    cPlayer <- use player
    let x'  = cPlayer^.pos.x + (cPlayer^.vel.x * (fromIntegral deltaTicks / 1000.0))
        y'  = cPlayer^.pos.y + (cPlayer^.vel.y * (fromIntegral deltaTicks / 1000.0))
        x'' = if x' < 0 then 0 else
                if (x' + fromIntegral 162) > fromIntegral screenWidth then fromIntegral $ screenWidth - 162 else x'
        y'' = if y' < 0 then 0 else
                if (y' + fromIntegral 150) > fromIntegral screenHeight then fromIntegral $ screenHeight - 150 else y'
    player.pos .= Pos x'' y''


intersects :: (Float, Float) -> Rect -> [Rect] -> Bool
intersects (cx,cy) Rect {rectX=px,rectY=py,rectW=pw,rectH=ph } list = checkRects list
  where t = truncate
        checkRects [] = False
        checkRects (Rect {rectX=bx,rectY=by,rectW=bw,rectH=bh }:xs) =
          if py + ph > by - t cy
          && py      < by + bh - t cy
          && px + pw > bx - t cx
          && px      < bx + bw - t cx
          then True
          else checkRects xs
