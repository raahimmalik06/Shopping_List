module Model where

import qualified Data.Map as M

-- Item category (ADT)
data Category
  = Grocery
  | Electronics
  | Clothing
  | Household
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Shopping item (record type)
data Item = Item
  { itemName     :: String
  , itemCategory :: Category
  , itemPrice    :: Double
  , itemQuantity :: Int
  } deriving (Eq, Show)

-- Shopping list (immutable map)
newtype ShoppingList = ShoppingList (M.Map String Item)
  deriving (Eq, Show)

-- Type class for polymorphic cost calculation
class Priced a where
  cost :: a -> Double

instance Priced Item where
  cost i = itemPrice i * fromIntegral (itemQuantity i)

instance Priced ShoppingList where
  cost (ShoppingList m) = sum (map cost (M.elems m))