Functional Shopping List Manager (Haskell)
Project Overview

This project was developed for PRG2214 – Functional Programming Principles.

It is a terminal-based Shopping List Management System implemented in Haskell to demonstrate key functional programming concepts such as:

Immutability

Algebraic Data Types (ADTs)

Type Classes and Polymorphism

Pure Functions

Monadic IO

Functional Decomposition

The program allows users to manage shopping items interactively using a structured menu system.

Features

The system supports the following operations:

Add Item (with validation)

Remove Item (with item-not-found feedback)

Update Quantity (validated)

View Shopping List (formatted table output)

Search for Item

Calculate Total Cost

All updates are performed immutably and return new versions of the shopping list.

Functional Programming Concepts Demonstrated
Immutability

The ShoppingList is never modified in place.
Each operation returns a new updated version of the list.

Pure Functions

All business logic (add, remove, update, search, total calculation) is implemented as pure functions in Logic.hs.

Algebraic Data Types

Category

Item

ShoppingList

Type Class and Polymorphism

Custom type class:

class Priced a where
  cost :: a -> Double

Implemented for:

Item

ShoppingList

This enables polymorphic cost calculation using the same function.

Monads

IO for user interaction

Either for validation and error handling

Maybe for safe search results

Higher-Order Functions

map

sum

mapM_

M.elems traversal

Pattern Matching

Used extensively in:

Menu handling

Validation

Case analysis

Project Structure
Project_<YourID>/
├── Main.hs      -- Entry point
├── Model.hs     -- Data types and type class definitions
├── Logic.hs     -- Pure functional core (no IO)
├── UI.hs        -- Terminal user interface (IO monad)
└── README.md
Module Responsibilities

Model.hs

Defines domain data types

Implements the Priced type class

Logic.hs

Contains all pure functions

Handles validation

Contains no IO operations

UI.hs

Handles user input and output

Calls pure logic functions

Displays formatted results

Main.hs

Program entry point

How to Compile and Run

Make sure GHC (Glasgow Haskell Compiler) is installed.

Compile
ghc Main.hs
Run (Mac/Linux)
./Main
Run (Windows)
Main.exe
Example Output
Menu
===== SHOPPING LIST MANAGER =====
1. Add Item
2. Remove Item
3. Update Quantity
4. View Shopping List
5. Search Item
6. Calculate Total Cost
0. Exit
Select option:
Formatted View
Item        Category       Price    Qty    Total
--------------------------------------------------
Milk        Grocery         2.50      2     5.00
T-Shirt     Clothing       15.00      1    15.00
--------------------------------------------------
Total Cost: $20.00
Libraries Used

Data.Map (containers package) — Immutable key-value storage

Text.Printf — Formatted console output

Only standard Haskell libraries are used to maintain portability and align with functional programming principles.

Design Highlights

Case-insensitive item keys

Strong validation logic (price ≥ 0, quantity > 0)

Clear separation of logic and UI

Modular multi-file structure

Explicit error handling with Either

Safe lookups using Maybe

Academic Context

This project was created for academic submission in PRG2214 and follows the functional programming principles taught in the module. The design emphasizes correctness, modularity, and functional purity over interface complexity.