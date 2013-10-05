module End.UI where

import Graphics.UI.SDL (Event(..), Keysym(..), SDLKey(..), pollEvent)
import End.Collection
import End.Collection.Header
import End.Object.Fireball(fireBall)
import End.Function.Object
import End.Collection.Util

handleInput :: Event -> GameState ()
handleInput (KeyDown (Keysym SDLK_w _ _)) = calcVel (-) y y
handleInput (KeyDown (Keysym SDLK_s _ _)) = calcVel (+) y y
handleInput (KeyDown (Keysym SDLK_a _ _)) = calcVel (-) x x
handleInput (KeyDown (Keysym SDLK_d _ _)) = calcVel (+) x x


handleInput (KeyUp   (Keysym SDLK_w _ _)) = calcVel (+) y y
handleInput (KeyUp   (Keysym SDLK_s _ _)) = calcVel (-) y y
handleInput (KeyUp   (Keysym SDLK_a _ _)) = calcVel (+) x x
handleInput (KeyUp   (Keysym SDLK_d _ _)) = calcVel (-) x x

handleInput (KeyDown (Keysym SDLK_SPACE _ _)) = fireBall >>= addObject temps

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
