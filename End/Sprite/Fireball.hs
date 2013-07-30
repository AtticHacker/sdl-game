module End.Sprite.Fireball where

import End.Header.Animation
import Graphics.UI.SDL
import End.Util.Animation

fireballSprite :: IO Sprite
fireballSprite = do
    s <- loadImage "images/Fireball" $ Just (0xee,0xff,0xde)
    return $ Sprite (pAnimation s) (Rect 0 0 0 0) (Rect 0 0 0 0)

pAnimation :: Surface -> Animation
pAnimation s = Animation s allActions bgGreen

allActions :: Allactions
allActions = Allactions stand' run' walk' attack'

stand' :: Maybe AAction
stand' = Just $ AAction 1 0 50 100 10 10 defaultOrder 1

run' :: Maybe AAction
run' = Nothing

walk' :: Maybe AAction
walk' = Just $ AAction 8 0 40 90 10 10 defaultOrder 10

attack' :: Maybe AAction
attack' = Nothing
