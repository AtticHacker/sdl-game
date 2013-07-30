module End.UI where

import Graphics.UI.SDL (Event(..), Keysym(..), SDLKey(..), pollEvent)
import End.Collection
import End.Collection.Header
import End.Collection.Object (goRightObject)
import End.Function.Object
import End.Sprite.Player (pSprite)

handleInput :: Event -> GameState ()
handleInput (KeyDown (Keysym SDLK_UP    _ _)) = calcVel (-) y y
handleInput (KeyDown (Keysym SDLK_DOWN  _ _)) = calcVel (+) y y
handleInput (KeyDown (Keysym SDLK_LEFT  _ _)) = calcVel (-) x x
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) = calcVel (+) x x
handleInput (KeyUp   (Keysym SDLK_UP    _ _)) = calcVel (+) y y
handleInput (KeyUp   (Keysym SDLK_DOWN  _ _)) = calcVel (-) y y
handleInput (KeyUp   (Keysym SDLK_LEFT  _ _)) = calcVel (+) x x
handleInput (KeyUp   (Keysym SDLK_RIGHT _ _)) = calcVel (-) x x

handleInput (KeyDown   (Keysym SDLK_SPACE _ _)) = fireBall >>= addObject temps

handleInput _ = return ()

calcVel func dVel dVelS = do
    pVel <- use $ player.vel.dVel
    mVel <- use $ player.maxVel
    let result = func pVel mVel
        r | result > 0 =   mVel
          | result < 0 = - mVel
          | otherwise  = 0
    player.vel.dVelS  .= r

whileEvents :: (Event -> GameState ()) -> GameState Bool
whileEvents a = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       -> a event >> whileEvents a

fireBall :: GameState Temp
fireBall = do
    objs    <- use $ objects.temps
    genId   <- liftIO $ generateID (objs^..traverse.iden)
    px <- use $ player.pos.x
    py <- use $ player.pos.y
    newT <- use newTick
    return $ Temp genId "FireBall" 5 goRightObject
        (SpriteStatus PlayerT Walk 0 newT)
        (Pos (px + 110) (py + 50)) (Vel 0 0) offScreen DRight
  where offScreen = (> 550) . (^.pos.x)
