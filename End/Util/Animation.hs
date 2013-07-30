module End.Util.Animation where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Data.Word
import End.Collection
import End.Collection.Header

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' :: Maybe (Word8, Word8, Word8) -> Surface -> IO Surface
setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = mapRGB' surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

--animationRectangles o = do
--    let max = o^.
--    create [0..o^.sprite.animation]
--    where create [] = []
--          create (xx:xs) = []