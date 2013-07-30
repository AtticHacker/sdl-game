module End.Header.Object where

import End.Collection
import End.Header.Animation

import qualified Data.ByteString.Char8 as B

data Obj = Obj
    { _objIden      :: Int
    , _objName      :: B.ByteString
    , _objSpriteStatus :: SpriteStatus
    , _objPos       :: Pos
    , _objVel       :: Vel
    , _objExpire    :: (Obj -> Bool)
    , _objFunction  :: (Word32 -> Obj -> Obj)
    , _objDirection :: Direction
    }

data Camera = Camera
              { _cameraX :: Float
              , _cameraY :: Float
              }

data Pos = Pos
           { _posX :: Float
           , _posY :: Float
           }

data Vel = Vel
           { _velX :: Float
           , _velY :: Float
           }

makeFields ''Camera
makeFields ''Obj
makeFields ''Pos
makeFields ''Vel

class ( HasIden a Int
      , HasName a B.ByteString
      , HasSpriteStatus a e
      , HasPos a Pos
      , HasVel a Vel
      , HasExpire a (a -> Bool)
      , HasFunction a (Word32 -> a -> a)
      , HasDirection a Direction
      ) => Object a e
