module End.Header.Object.Temp where

import qualified Data.ByteString.Char8 as B

import End.Collection
import End.Header.Animation
import End.Header.Object
data Temp = Temp
              { _tempIden     :: Int
              , _tempName     :: B.ByteString
              , _tempLifeTime :: Int
              , _tempFunction :: (Word32 -> Temp -> Temp)
              , _tempSprite   :: Sprite
              , _tempPos      :: Pos
              , _tempVel      :: Vel
              , _tempExpire   :: (Temp -> Bool)
              }

instance Object Temp Sprite

makeFields ''Temp
