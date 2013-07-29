module End.Util.Random where

import System.Random

randomPerc :: Int -> IO Bool
randomPerc i = do
    r <- getStdRandom $ randomR (1,100)
    if r <= i then return True else return False

randomInt :: Int -> Int -> IO Int
randomInt i i2 = getStdRandom $ randomR (i,i2)

randomFloat :: Float -> Float -> IO Float
randomFloat i i2 = getStdRandom $ randomR (i,i2)
