module End.Header.Tile where

import End.Collection
import End.Collection.Header

data Tile = Tile
            { _tileIden :: Int
            , _tileCollision :: Int
            }

type CollisionCords = (Int,Int,Int,Int)

makeFields ''Tile

getCollision :: Tile -> CollisionCords
getCollision tile = case tile^.collision of
    0  -> (0,  0,  0,  0  )
    1  -> (0,  0,  32, 32 )
    2  -> (0,  0,  24, 32 )
    3  -> (0,  0,  16, 32 )
    4  -> (0,  0,  8,  32 )
    5  -> (0,  0,  1,  32 )
    6  -> (8,  0,  24, 32 )
    7  -> (16, 0,  16, 32 )
    8  -> (24, 0,  8,  32 )
    9  -> (31, 0,  1,  32 )
    10 -> (0,  0,  32, 24 )
    11 -> (0,  0,  32, 16 )
    12 -> (0,  0,  32, 8  )
    13 -> (0,  0,  32, 1  )
    14 -> (0,  8,  32, 24 )
    15 -> (0,  16, 32, 16 )
    16 -> (0,  24, 32, 8  )
    17 -> (0,  31, 32, 1  )
    18 -> (4,  0,  24, 32 )
    19 -> (8,  0,  16, 32 )
    20 -> (12, 0,  8,  32 )
    21 -> (15, 0,  2,  32 )
    22 -> (0,  4,  32, 24 )
    23 -> (0,  8,  32, 16 )
    24 -> (0,  12, 32, 8  )
    25 -> (0,  15, 32, 2  )
    26 -> (0,  16, 16, 16 )
    27 -> (0,  0,  16, 16 )
    28 -> (16, 0,  16, 16 )
    29 -> (16, 16, 16, 16 )
    30 -> (8,  8,  16, 16 )
    i -> error $ "Incorrect Collision id " ++ show i
