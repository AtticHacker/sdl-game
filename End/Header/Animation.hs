module End.Header.Animation where

import End.Collection
import Graphics.UI.SDL

type BGColor = (Word8, Word8, Word8)

data SpriteTag = PlayerT
               | FireballT
               deriving Eq

data Direction = DDynamic
               | DUp
               | DRight
               | DDown
               | DLeft
               deriving (Show, Read, Ord, Eq, Enum)

dirId :: Direction -> Int
dirId a = isDir (-1) DDynamic
  where isDir i n
            | a == DDynamic = 0
            | n == a        = i
            | otherwise     =
                isDir (i+1) (succ n)

data ActionTag =
    Stand
    | Run
    | Walk
    | Attack
    deriving Eq

data Sprite = Sprite
    { _spriteAnimation    :: Animation
    }

data Animation = Animation
    { _animationImage   :: Surface
    , _animationActions :: [(ActionTag, AAction)]
    , _animationBg      :: BGColor
    }

data AAction = AAction
    { _actionMaxFrames :: Int
    , _actionX         :: Float
    , _actionW         :: Int
    , _actionH         :: Int
    , _actionFps       :: Int
    , _actionBox       :: CollisionBox
    }

data CollisionBox = CollisionBox
    { _collisionboxX :: Float
    , _collisionboxY :: Float
    , _collisionboxW :: Int
    , _collisionboxH :: Int
    }

data SpriteStatus = SpriteStatus
    { _spritestatusTag       :: SpriteTag
    , _spritestatusActionTag :: ActionTag
    , _spritestatusFrame     :: Int
    , _spritestatusLastFrame :: Word32
    }

makeFields ''Animation
makeFields ''Sprite
makeFields ''AAction
makeFields ''SpriteStatus
makeFields ''CollisionBox

bgGreen :: BGColor
bgGreen = (0xee,0xff,0xde)