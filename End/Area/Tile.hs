
module End.Area.Tile where

import End.Collection.Header
import End.Collection
import End.Constant.Settings
import Graphics.UI.SDL
import End.Global.Drawing
import End.Area.Tiles
import Data.Maybe
import Data.Foldable hiding (elem)
import End.Collection.Util
import End.Function.Object
import Control.Parallel.Strategies hiding (parPair)

parPair :: Strategy (a,b)
parPair (a,b) = do
    a' <- rpar a
    b' <- rpar b
    return (a',b')

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

-- drawAllTiles :: Surface -> GameState ()
-- drawAllTiles dst = do
--     t <- view tileSet
--     m <- view gameMap
--     Camera cx cy <- use $ player.camera
--     ((sx, ex), (sy,ey)) <- getTilesToDraw
--     let (required1, required2) = splitHalf $ zip (drop sy $ take ey m) [sy..ey]
-- --    void $ drawRows 0 (sx,ex) required (cx, cy) t dst

--     let (xx,xs) = splitHalf $ drawRowsList 0 (sx,ex) required (cx, cy) t dst
--     let func = mapM (\(a,b,c) -> applySurface a b t dst c)
--     let (q,e) = ((func xx), (func xs)) `using` parPair
--     liftIO q
--     liftIO e
--     return ()

drawAllTiles :: Surface -> GameState ()
drawAllTiles dst = do
    t <- view tileSet
    m <- view gameMap
    Camera cx cy <- use $ player.camera
    ((sx, ex), (sy,ey)) <- getTilesToDraw
    let (required1, required2) = splitHalf $ zip (drop sy $ take ey m) [sy..ey]
--    void $ drawRows 0 (sx,ex) required (cx, cy) t dst
--    let (xx,xs) = splitHalf $ drawRowsList 0 (sx,ex) required (cx, cy) t dst
    let func1 a = drawRowsList 0 (sx,ex) a (cx, cy) t dst
    let (xx,xs) = (func1 required1, func1 required2) `using` parPair
    let func = mapM (\(a,b,c) -> applySurface a b t dst c)
    let (q,e) = ((func xx), (func xs)) `using` parPair
    liftIO q
    liftIO e
    return ()

drawRowsList _ _ [] _ _ _          = []
drawRowsList _ dw (([],_):o) cxy src dst = drawRowsList 0 dw o cxy src dst
drawRowsList i dw@(sx,ex) (((xx:xs),dh):o) (cx,cy) src dst
    | i > ex = drawRowsList 0 dw o (cx, cy) src dst
    | i < sx = drawRowsList (i+1) dw ((xs,dh):o) (cx, cy) src dst
    | otherwise = do
        ((tileBit*i - truncate cx), (tileBit*dh - truncate cy), (getTile xx)) :
            drawRowsList (i+1) dw ((xs,dh):o) (cx, cy) src dst


drawRows :: Int -> (Int, Int) -> [([Int], Int)] ->
            (Float, Float) -> Surface -> Surface -> GameState ()
drawRows _ _ [] _ _ _          = return ()
drawRows _ dw (([],_):o) cxy src dst = drawRows 0 dw o cxy src dst
drawRows i dw@(sx,ex) (((xx:xs),dh):o) (cx,cy) src dst
    | i > ex = drawRows 0 dw o (cx, cy) src dst
    | i < sx = drawRows (i+1) dw ((xs,dh):o) (cx, cy) src dst
    | otherwise = do
        void $ liftIO $ applySurface (tileBit*i - truncate cx)
            (tileBit*dh - truncate cy) src dst $ getTile xx
        drawRows (i+1) dw ((xs,dh):o) (cx, cy) src dst

getTilesToDraw :: GameState ((Int, Int), (Int, Int))
getTilesToDraw = do
    ((sx, ex), (sy, ey)) <- playerView
    let  dBy a = div a tileBit
         rsx = dBy sx
         rex = dBy ex
         rsy = dBy sy
         rey = dBy ey + 1
    return $ ((rsx, rex), (rsy, rey))

playerView :: GameState ((Int, Int), (Int, Int))
playerView = do
    Camera cx cy <- use $ player.camera
    let t = truncate
    return $ ( (t cx, t cx + screenWidth)
             , (t cy, t cy + screenHeight))

getTile :: Int -> Maybe Rect
getTile i   = Just $ Rect (colomn*tileBit) (row*tileBit) tileBit tileBit
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

makeCollisionRects :: Map -> [RectID]
makeCollisionRects theMap = do
  let required = zip theMap [0..mapHeight]
  addColl 0 (0,mapWidth) required

  where addColl _ _ []          = []
        addColl _ ww (([],_):o) = addColl 0 ww o
        addColl i ww@(sx,ex) (((xx:xs),hh):o)
          | i > ex = addColl 0 ww o
          | i < sx || xx == 0 = addColl (i+1) ww ((xs,hh):o)
          | otherwise = (xx, Rect (i*32 + getX xx)
                             (hh*32 + getY xx) (getW xx) (getH xx)) :
                           addColl (i+1) (sx,ex) ((xs,hh):o)
        getX = (^._1) . getCollTup
        getY = (^._2) . getCollTup
        getW = (^._3) . getCollTup
        getH = (^._4) . getCollTup

getRectsFromID :: [RectID] -> [Rect]
getRectsFromID xs = map snd xs

combine :: Char -> [RectID] -> [RectID]
combine _ [] = []
combine t (xx:xs) = combineWorker xx xs
  where combineWorker :: RectID -> [RectID] -> [RectID]
        combineWorker a [] = a : []
        combineWorker oldT xxsr@(xr:xsr) =
          if (nextRect oldT) `elem` xxsr
            then combineWorker (addToRect oldT) $
                 filter (/=(nextRect oldT)) xxsr
            else oldT : combineWorker xr xsr
        xList = 1:[10..17]++[22..25]
        yList = [1..9]++[18..21]
        nextRect (i, a)
          | t == 'X' && i `elem` xList =
              (i, a { rectW=(32), rectX=(rectX a + rectW a) })
          | t == 'Y' && i `elem` yList =
              (i, a { rectH=(32), rectY=(rectY a + rectH a) })
          | otherwise = (i, a { rectX=(0), rectY=(0), rectW=(0), rectH=(0) })

        addToRect (i, a)
          | t == 'X' = (i, a{ rectW=(rectW a + 32)})
          | t == 'Y' = (i, a{ rectH=(rectH a + 32)})
          | otherwise = error "Error in combine addToRect"

filterRects :: [Rect] -> [Rect]
filterRects [] = []
filterRects (xx:xs)
    | rectW xx == 0 && rectH xx == 0 = filterRects xs
    | otherwise                      = xx : filterRects xs
splitMap :: Int -> [Int] -> Map
splitMap _ [] = []
splitMap i xxs = a : splitMap i b
    where (a,b) = splitAt i xxs


mapToColMap :: Map -> Map
mapToColMap a = a&traverse.traverse %~ (\b -> fromJust $ lookup b tileList)

drawColls :: Surface -> GameState ()
drawColls s = do
    p <- use player
    Just sp <- getSpriteAction p
    c <- view colls >>= return . filterNearbyTiles p sp
    Camera cx cy <- use $ player.camera
    color <- liftIO $ mapRGB' s 0x00 0x00 0x00
    void $ liftIO $ for_ c $ \d -> do
        fillRect s (Just (d { rectX =(rectX d - (truncate cx))
                                     , rectY =(rectY d - (truncate cy))
                                     })) color

filterNearbyTiles :: Object o SpriteStatus => o -> AAction -> [Rect] -> [Rect]
filterNearbyTiles _ _ [] = []
filterNearbyTiles o sp (xx@(Rect rx ry rw rh):xs)
  | (rx + rw + 10 < ox)              = next
  | (rx > ox + sp^.box.w + spx) = next
  | (ry + rh + 10 < oy)              = next
  | (ry > oy + sp^.box.h + spy) = next
  | otherwise             = xx : next
  where ox  = truncate $ o^.pos.x
        oy  = truncate $ o^.pos.y
        spx = 10 + (fromEnum $ sp^.box.x)
        spy = 10 + (fromEnum $ sp^.box.y)
        next = filterNearbyTiles o sp xs
