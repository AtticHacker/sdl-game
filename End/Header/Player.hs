module End.Header.Player where

import qualified Data.ByteString.Char8 as B

import End.Header.Animation
import End.Collection
import End.Header.Object
import Graphics.UI.SDL

data Player = Player
              { _playerIden         :: Int
              , _playerName         :: B.ByteString
              , _playerMp           :: Int
              , _playerHp           :: Int
              , _playerRect         :: Rect
              , _playerPos          :: Pos
              , _playerVel          :: Vel
              , _playerMaxVel       :: Float
              , _playerCamera       :: Camera
              , _playerDirection    :: Direction
              , _playerSpriteStatus :: SpriteStatus
              , _playerExpire       :: (Player -> Bool)
              , _playerFunction     :: (Word32 -> Player -> Player)
              }

makeFields ''Player
instance Object Player SpriteStatus
