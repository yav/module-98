module Types.Names
  (Name(..), QName, ModName(..)
  , getQualifier, getQualified
  , mkUnqual, mkQual
  ) where


-- Interface of the module system, to the concrete type of names

newtype Name    = Name String                     deriving (Eq,Ord)
newtype ModName = ModName String                  deriving (Eq,Ord)
data QName      = Qual ModName Name | Unqual Name deriving (Eq,Ord)


getQualifier (Qual q _) = Just q
getQualifier (Unqual _) = Nothing

getQualified (Qual _ n) = n
getQualified (Unqual n) = n

mkUnqual  = Unqual
mkQual    = Qual


instance Show Name where
  show (Name x) = x

instance Show ModName where
  show (ModName x) = x

instance Show QName where
  show (Qual x y) = show x ++ "." ++ show y
  show (Unqual x) = show x
