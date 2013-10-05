module End.Header.Tile where

import End.Collection
-- import End.Collection.Header
import Graphics.UI.SDL

type Map = [[Int]]

data Tile = Tile
            { _tileIdent     :: Int
            , _tileCollision :: Rect
            }

type Tiles = [Tile]

type CollisionCords = (Int,Int,Int,Int)

type RectID = (Int,Rect)

makeFields ''Tile

lookupCol :: Int -> [Tile] -> Maybe Rect
lookupCol _ [] = Nothing
lookupCol i' ((Tile i c):xs)
    | i' == i = Just c
    | otherwise = lookupCol i' xs

getCollTup :: Int -> CollisionCords
getCollTup = fromRect . getColl

fromRect :: Rect -> (Int,Int,Int,Int)
fromRect (Rect a b c d) = (a,b,c,d)

getColl :: Int -> Rect
getColl 0  = Rect 0  0  0  0
getColl 1  = Rect 0  0  32 32
getColl 2  = Rect 0  0  24 32
getColl 3  = Rect 0  0  16 32
getColl 4  = Rect 0  0  8  32
getColl 5  = Rect 0  0  1  32
getColl 6  = Rect 8  0  24 32
getColl 7  = Rect 16 0  16 32
getColl 8  = Rect 24 0  8  32
getColl 9  = Rect 31 0  1  32
getColl 10 = Rect 0  0  32 24
getColl 11 = Rect 0  0  32 16
getColl 12 = Rect 0  0  32 8
getColl 13 = Rect 0  0  32 1
getColl 14 = Rect 0  8  32 24
getColl 15 = Rect 0  16 32 16
getColl 16 = Rect 0  24 32 8
getColl 17 = Rect 0  31 32 1
getColl 18 = Rect 4  0  24 32
getColl 19 = Rect 8  0  16 32
getColl 20 = Rect 12 0  8  32
getColl 21 = Rect 15 0  2  32
getColl 22 = Rect 0  4  32 24
getColl 23 = Rect 0  8  32 16
getColl 24 = Rect 0  12 32 8
getColl 25 = Rect 0  15 32 2
getColl 26 = Rect 0  16 16 16
getColl 27 = Rect 0  0  16 16
getColl 28 = Rect 16 0  16 16
getColl 29 = Rect 16 16 16 16
getColl 30 = Rect 8  8  16 16
getColl i  = error $ "Incorrect Collision id " ++ show i
