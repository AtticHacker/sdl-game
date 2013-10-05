module End.Util.State where

import End.Collection

setS :: MonadState s m => ASetter s s a b -> b -> m ()
setS = (.=)

(##) :: MonadState s m => Getting t s t -> (t -> b) -> m b
a ## f = use a >>= return . f

(###) :: MonadState s m => Getting a s a -> (a -> m b) -> m b
a ### f = use a >>= f

infixl 1 ##
infixl 1 ###


-- examples
-- player.iden ## (+10) #! (*0) >>= (player.iden .=)
-- player.iden ## print >>= liftIO
-- player.iden ### (player.iden .=)