module End.Object.Collection where

import End.Object.Temp
import Control.Lens

data Objectlist = Objectlist
    { _objectlistTemps :: [Temp] }

makeFields ''Objectlist