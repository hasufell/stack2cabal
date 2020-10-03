module StackageToHackage.Prelude where

import Control.Applicative (Alternative, empty)


hoistMaybe :: Alternative m => Maybe a -> m a
hoistMaybe = maybe empty pure
