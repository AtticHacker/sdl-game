module End.Object.Temp where

import qualified Data.ByteString.Char8 as B

import End.Collection
import End.Header.Animation
import End.Header.Object
import End.Global.Object
import End.Header.Object.Temp
import End.Header.Config
import Data.Word
import Graphics.UI.SDL.Time as SdlTime
import System.IO.Unsafe

goRightObject a = (& pos.x +~ (300 * (fromIntegral a / 1000.0)))
