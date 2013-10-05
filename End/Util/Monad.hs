module End.Util.Monad where


revMapM :: Monad m => t -> [t -> m ()] -> m ()
revMapM _ []      = return ()
revMapM f [xx]    = xx f
revMapM f (xx:xs) = xx f >> revMapM f xs

revForM :: Monad m => [f -> m ()] -> f -> m ()
revForM [] _      = return ()
revForM [xx]    f = xx f
revForM (xx:xs) f = xx f >> revForM xs f
