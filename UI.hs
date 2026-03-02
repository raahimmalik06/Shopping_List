module UI where

import Model
import Logic
import qualified Data.Map as M
import Text.Printf

runApp :: IO ()
runApp = loop emptyList

loop :: ShoppingList -> IO ()
loop list = do
  printMenu
  choice <- getLine
  case choice of
    "1" -> addUI list >>= loop
    "2" -> removeUI list >>= loop
    "3" -> updateUI list >>= loop
    "4" -> viewUI list >> loop list
    "5" -> searchUI list >> loop list
    "6" -> totalUI list >> loop list
    "0" -> putStrLn "Thank you for using the Shopping List Manager."
    _   -> putStrLn "Invalid choice. Please try again." >> loop list

printMenu :: IO ()
printMenu = do
  putStrLn "\n===== SHOPPING LIST MANAGER ====="
  putStrLn "1. Add Item"
  putStrLn "2. Remove Item"
  putStrLn "3. Update Quantity"
  putStrLn "4. View Shopping List"
  putStrLn "5. Search Item"
  putStrLn "6. Calculate Total Cost"
  putStrLn "0. Exit"
  putStr "Select option: "

-------------------------------------------------
-- UI OPERATIONS
-------------------------------------------------

addUI :: ShoppingList -> IO ShoppingList
addUI list = do
  putStr "Item name: "
  name <- getLine
  cat  <- readCategory
  price <- readDouble "Price (>= 0): "
  qty   <- readInt "Quantity (> 0): "
  let item = Item name cat price qty
  case addItem item list of
    Left err -> putStrLn ("Error: " ++ err) >> return list
    Right nl -> putStrLn "Item added successfully." >> return nl

removeUI :: ShoppingList -> IO ShoppingList
removeUI list = do
  putStr "Item name to remove: "
  name <- getLine
  case removeItem name list of
    Left err -> putStrLn ("Error: " ++ err) >> return list
    Right nl -> putStrLn "Item removed successfully." >> return nl

updateUI :: ShoppingList -> IO ShoppingList
updateUI list = do
  putStr "Item name to update: "
  name <- getLine
  qty  <- readInt "New quantity (> 0): "
  case updateQuantity name qty list of
    Left err -> putStrLn ("Error: " ++ err) >> return list
    Right nl -> putStrLn "Quantity updated successfully." >> return nl

viewUI :: ShoppingList -> IO ()
viewUI list =
  case listItems list of
    [] -> putStrLn "Shopping list is empty."
    xs -> do
      putStrLn "\nItem        Category       Price    Qty    Total"
      putStrLn "--------------------------------------------------"
      mapM_ printItem xs
      putStrLn "--------------------------------------------------"
      printf "Total Cost: $%.2f\n" (cost list)

searchUI :: ShoppingList -> IO ()
searchUI list = do
  putStr "Item name to search: "
  name <- getLine
  case searchItem name list of
    Nothing -> putStrLn "Item not found."
    Just i  -> do
      putStrLn "\nItem        Category       Price    Qty    Total"
      putStrLn "--------------------------------------------------"
      printItem i

totalUI :: ShoppingList -> IO ()
totalUI list =
  printf "Total cost: $%.2f\n" (cost list)

-------------------------------------------------
-- HELPERS (IO parsing + formatting)
-------------------------------------------------

printItem :: Item -> IO ()
printItem i =
  printf "%-10s  %-12s  %7.2f  %5d  %7.2f\n"
    (trim10 (itemName i))
    (show (itemCategory i))
    (itemPrice i)
    (itemQuantity i)
    (cost i)

trim10 :: String -> String
trim10 s = take 10 s

readCategory :: IO Category
readCategory = do
  putStrLn "Category: 1.Grocery 2.Electronics 3.Clothing 4.Household"
  putStr "Select category: "
  c <- getLine
  case c of
    "1" -> return Grocery
    "2" -> return Electronics
    "3" -> return Clothing
    "4" -> return Household
    _   -> putStrLn "Invalid category. Try again." >> readCategory

readDouble :: String -> IO Double
readDouble msg = do
  putStr msg
  input <- getLine
  case reads input of
    [(x, "")] -> return x
    _         -> putStrLn "Invalid number. Try again." >> readDouble msg

readInt :: String -> IO Int
readInt msg = do
  putStr msg
  input <- getLine
  case reads input of
    [(x, "")] -> return x
    _         -> putStrLn "Invalid integer. Try again." >> readInt msg