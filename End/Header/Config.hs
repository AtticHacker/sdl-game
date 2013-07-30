module End.Header.Config where

import Control.Lens
import Graphics.UI.SDL
import Control.Monad.State
import Control.Monad.Reader

import End.Header.Player
import End.Header.Animation
import End.Header.Objectlist
import Data.Word

data Gamestate = Gamestate
                 { _gamestatePlayer   :: Player
                 , _gamestateObjects  :: Objectlist
                 , _gamestateOldTick  :: Word32
                 , _gamestateNewTick  :: Word32
                 }

data Gameconfig = Gameconfig
                  { _gameconfigScreen     :: Screen
                  , _gameconfigGameScreen :: Surface
                  , _gameconfigImages     :: [(SpriteTag, Sprite)]
                  }

data Screen = Screen
              { _screenW :: Int
              , _screenH :: Int
              } deriving Show

type GameState = ReaderT Gameconfig (StateT Gamestate IO)

makeFields ''Gamestate
makeFields ''Gameconfig
makeFields ''Screen
