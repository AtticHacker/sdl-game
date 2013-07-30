module End.Constant.Settings where

screenWidth      :: Int
screenHeight     :: Int
halfScreenWidth  :: Int
halfScreenHeight :: Int
screenBpp        :: Int
mapWidth         :: Int
mapHeight        :: Int
mapWidthPx       :: Int
mapHeightPx      :: Int

screenWidth      = 800
screenHeight     = 600
halfScreenWidth  = screenWidth  `div` 2
halfScreenHeight = screenHeight `div` 2
screenBpp        = 32
mapWidth         = 100
mapHeight        = 100
mapWidthPx       = mapWidth  * 32
mapHeightPx      = mapHeight * 32
playerVel :: Float
playerVel = 200