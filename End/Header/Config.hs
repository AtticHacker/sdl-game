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
                 { _gamestatePlayer    :: Player
                 , _gamestateObjects   :: Objectlist
                 , _gamestateDelta     :: Word32
--                 , _gamestateFrameRate :: Word32
                 }

data Gameconfig = Gameconfig
                  { _gameconfigScreen :: Screen
                  , _gameconfigGameScreen :: Surface
                  } deriving Show

data Screen = Screen
              { _screenW :: Int
              , _screenH :: Int
              } deriving Show

type GameState = ReaderT Gameconfig (StateT Gamestate IO)

makeFields ''Gamestate
makeFields ''Gameconfig
makeFields ''Screen
