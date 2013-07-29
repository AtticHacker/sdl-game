module End.Header.Animation where

import End.Collection
import Graphics.UI.SDL
import GHC.Word

type BGColor = (Word8, Word8, Word8)

data Sprite = Sprite
              { _spriteAnimation    :: Animation
              , _spriteHitBox       :: Rect
              , _spriteCollisionBox :: Rect
              }

data Direction = DDownLeft
               | DDown
               | DDownRight
               | DLeft
               | DRight
               | DUpLeft
               | DUp
               | DUpRight

data Animation = Animation
                 { _animationImage   :: FilePath
                 , _animationActions :: Allactions
                 , _animationBg      :: BGColor
                 }

data Allactions = Allactions
                  { _allactionsStand  :: Maybe AAction
                  , _allactionsRun    :: Maybe AAction
                  , _allactionsWalk   :: Maybe AAction
                  , _allactionsAttack :: Maybe AAction
                  }

data AAction = AAction
               { _actionMaxFrames :: Int
               , _actionX         :: Float
               , _actionW         :: Float
               , _actionH         :: Float
               , _actionMw        :: Float
               , _actionHw        :: Float
               , _actionOrder     :: [Direction]
               , _actionFPS       :: Int
               }

makeFields ''Animation
makeFields ''Sprite
makeFields ''AAction
makeFields ''Allactions

defaultOrder :: [Direction]
defaultOrder = [ DDownLeft,  DDown
               , DDownRight, DLeft
               , DRight,     DUpLeft
               , DUp,        DUpRight ]

bgGreen :: BGColor
bgGreen = (0xee,0xff,0xde)