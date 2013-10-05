module End.Function.Object where

import End.Collection.Util
import End.Collection.Header
import End.Collection

isExpired :: (Object a b) => [a] -> [a]
isExpired [] = []
isExpired (xx:xs)
    | (xx^.expire) xx = isExpired xs
    | otherwise       = xx : isExpired xs

addObject :: (MonadState s m, HasObjects s e) =>
             (([a] -> Mutator [a]) -> e -> Mutator e) ->
             a -> m ()
addObject s o = objects.s %= (o:)

removeObj :: (Object t a) => t -> [t] -> [t]
removeObj _ [] = []
removeObj a (x':xs)
    | a^.iden /= x'^.iden = x' : removeObj a xs
    | otherwise           = foldr (\o acc -> o : acc)
                            xs $ removeObj a []

runOnSelf :: (a -> a -> b) -> a -> b
runOnSelf f a = (f a) a

generateID :: [Int] -> IO Int
generateID xs = do
    rand <- randomInt 1 10000
    return $ generateID' rand xs

generateID' :: Int -> [Int] -> Int
generateID' _ [] = 1
generateID' i xs
    | i `elem` xs = generateID' (i+1) xs
    | otherwise   = i

getSprite :: (Object o SpriteStatus) => o -> GameState (Maybe Sprite)
getSprite b = view images >>= return . lookup (b^.spriteStatus.tag)

getSpriteAction :: (Object o SpriteStatus) => o -> GameState (Maybe AAction)
getSpriteAction b = do
    Just sp <- getSprite b
    return $ lookup (b^.spriteStatus.actionTag) (sp^.animation.actions)
