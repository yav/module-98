module Types.Ents(Entity,owns,isCon,mkEnt {- for testing -}) where

import Data.Char
import Util.Set


-- interface of the module system to the concrete type of entities

data Entity = Entity 
  { definedIn :: String
  , name      :: String
  , owns      :: Set Entity
  }

instance Show Entity where
  show x = name x ++ " of " ++ definedIn x

instance Eq Entity where
  e1 == e2      = toPair e1 == toPair e2

instance Ord Entity where
  compare e1 e2 = compare (toPair e1) (toPair e2)


isCon (Entity { name = x:_ }) = isUpper x
isCon _                       = False


mkEnt a b c = Entity { definedIn = a, name = b, owns = c}



toPair ent = (name ent, definedIn ent)
 

