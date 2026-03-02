module Logic where

import Model
import qualified Data.Map as M
import Data.Char (toLower)

emptyList :: ShoppingList
emptyList = ShoppingList M.empty

keyOf :: String -> String
keyOf = map toLower

validateItem :: Item -> Either String Item
validateItem i
  | null (itemName i)       = Left "Item name cannot be empty."
  | itemPrice i < 0         = Left "Price must be >= 0."
  | itemQuantity i <= 0     = Left "Quantity must be > 0."
  | otherwise               = Right i

addItem :: Item -> ShoppingList -> Either String ShoppingList
addItem item (ShoppingList m) = do
  i <- validateItem item
  let k = keyOf (itemName i)
  Right (ShoppingList (M.insert k i m))

removeItem :: String -> ShoppingList -> Either String ShoppingList
removeItem name (ShoppingList m)
  | null name = Left "Item name cannot be empty."
  | M.member k m = Right (ShoppingList (M.delete k m))
  | otherwise = Left "Item not found."
  where
    k = keyOf name

updateQuantity :: String -> Int -> ShoppingList -> Either String ShoppingList
updateQuantity name qty (ShoppingList m)
  | null name   = Left "Item name cannot be empty."
  | qty <= 0    = Left "Quantity must be > 0."
  | otherwise =
      case M.lookup k m of
        Nothing -> Left "Item not found."
        Just i  -> Right (ShoppingList (M.insert k (i { itemQuantity = qty }) m))
  where
    k = keyOf name

searchItem :: String -> ShoppingList -> Maybe Item
searchItem name (ShoppingList m)
  | null name  = Nothing
  | otherwise  = M.lookup (keyOf name) m

listItems :: ShoppingList -> [Item]
listItems (ShoppingList m) = M.elems m