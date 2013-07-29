module End.Area.Tile where

import End.Collection.Header

mapWidth         = 100
mapHeight        = 100

createMap :: String -> IO [[Int]]
createMap string = do
  a <- readFile string
  let b = (read a :: [Int])
  return $ getSplit b
  where getSplit []   = []
        getSplit xs   = a : getSplit b
          where (a,b) = splitAt mapWidth xs

getTilesToDraw :: Player -> ((Int, Int), (Int, Int))
getTilesToDraw player =
  ((rsx, rex), (rsy, rey))
  where dBy32 a = div a 32
        ((sx, ex), (sy, ey)) = playerView player
        rsx = dBy32 sx
        rex = dBy32 ex
        rsy = dBy32 sy
        rey = dBy32 ey +1

getTile :: Int -> Maybe Rect
getTile i   = Just $ Rect (colomn*32) (row*32) 32 32
  where str = (show i)
        (colomn, row)
          | i <  10   = (readLast, 0)
          | i == 10   = (9,0)
          | i == 20   = (9,1)
          | i == 30   = (9,2)
          | i == 40   = (9,3)
          | i == 50   = (9,4)
          | i == 60   = (9,5)
          | i == 70   = (9,6)
          | i == 80   = (9,7)
          | i == 90   = (9,8)
          | i == 100  = (9,9)
          | otherwise = (readLast, readHead)
        readHead = (read [(head str)] :: Int)
        readLast = (read [(last str)] :: Int) - 1
