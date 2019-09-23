module Util.Set where

import qualified Data.Set as S

type Set            = S.Set

mkSet xs            = S.fromList xs
setToList s         = S.toList s
emptySet            = S.empty
mapSet f xs         = S.fromList (map f (S.elems xs))
intersect s t       = S.intersection s t
unionManySets xs    = S.unions xs
minusSet s t        = S.difference s t
elementOf s t       = S.member s t
