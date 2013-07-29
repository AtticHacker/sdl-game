module End.UI where

import Graphics.UI.SDL (Event(..), Keysym(..), SDLKey(..), pollEvent)
import End.Collection
import End.Collection.Header
import End.Collection.Object (addObject, generateID, goRightObject)
import End.Sprite.Player (pSprite)

dotVel' :: Float
dotVel' = 300

handleInput :: Event -> GameState ()
-- handleInput (KeyDown (Keysym SDLK_w _ _))     = player.vel.y .= 10
-- handleInput (KeyDown (Keysym SDLK_SPACE _ _)) = fireBall >>= addObject temps
handleInput (KeyDown (Keysym SDLK_UP    _ _)) = player.vel.y .= (-dotVel')
handleInput (KeyDown (Keysym SDLK_DOWN  _ _)) = player.vel.y .= dotVel'
handleInput (KeyDown (Keysym SDLK_LEFT  _ _)) = player.vel.x .= (-dotVel')
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) = player.vel.x .= dotVel'

handleInput (KeyUp   (Keysym SDLK_UP    _ _)) = player.vel.y .= 0
handleInput (KeyUp   (Keysym SDLK_DOWN  _ _)) = player.vel.y .= 0
handleInput (KeyUp   (Keysym SDLK_LEFT  _ _)) = player.vel.x .= 0
handleInput (KeyUp   (Keysym SDLK_RIGHT _ _)) = player.vel.x .= 0

handleInput (KeyDown   (Keysym SDLK_SPACE _ _)) = fireBall >>= addObject temps

handleInput _ = return ()

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

    return $ Temp genId "FireBall" 5 goRightObject
        pSprite (Pos (px + 110) (py + 50)) (Vel 0 0) offScreen
  where offScreen = (> 550) . (^.pos.x)
