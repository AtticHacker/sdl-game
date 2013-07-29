module End.Header.Objectlist where

import End.Collection
import End.Header.Object.Temp

data Objectlist = Objectlist
    { _objectlistTemps :: [Temp] }

makeFields ''Objectlist
