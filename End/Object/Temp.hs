module End.Object.Temp where

import End.Collection
import End.Header.Animation
import End.Header.Object
import End.Header.Object.Temp

-- goRightObject :: (Eq a, Fractional a2, Integral a3, Num a, Ord a2, HasX e a2, HasPos a1 e, HasLifeTime a1 a) => a3 -> a1 -> a1
-- goRightObject a o = do
-- --    o & pos.x +~ (300 * (fromIntegral a / 1000.0))
--     let k = if (o^.pos.x) > 590 then
--            o&lifeTime .~ 0
--             else if (o^.pos.x) < 0 then
--                    o&lifeTime .~ 1
--                  else o

--     if k^.lifeTime == 0
--         then k & pos.x +~ ((-300) * (fromIntegral a / 1000.0))
--         else k & pos.x +~ (300 * (fromIntegral a / 1000.0))