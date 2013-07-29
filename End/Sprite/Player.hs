module End.Sprite.Player where

import End.Header.Animation
import Graphics.UI.SDL

pSprite :: Sprite
pSprite = Sprite pAnimation (Rect 0 0 0 0) (Rect 0 0 0 0)

pAnimation :: Animation
pAnimation = Animation "images/nya.png" allActions bgGreen

allActions :: Allactions
allActions = Allactions playerStand playerRun playerWalk playerAttack

playerStand :: Maybe AAction
playerStand  = Just $ AAction 1 0 50 100 10 10 defaultOrder 1

playerRun :: Maybe AAction
playerRun    = Nothing

playerWalk :: Maybe AAction
playerWalk   = Just $ AAction 8 0 40 90 10 10 defaultOrder 10

playerAttack :: Maybe AAction
playerAttack = Nothing
