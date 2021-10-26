module Main where

type Item = String
type Items = [Item]

-- Returns a new list of Items with the new item in it
addItem :: Item -> Items -> Items
addItem item items = item : items

-- Returns a string representation of the items
displayItems :: Items -> String
displayItems items =
    let
        displayItem index item = show index ++ " - " ++ item
        reversedList = reverse items
        displayedItemsList = zipWith displayItem [1..] reversedList
    in
        unlines displayedItemsList

-- returns a new list of items or an error essage if the index is out of bounds
-- removeItem :: Int -> Items -> Either String Items

-- Takes a list of items
-- Interact with the user
-- Return an updated list of items
interactWithUser :: Items -> IO Items
interactWithUser items = do
    putStrLn "Enter an item to add to your todo list:"
    item <- getLine
    let newItems = addItem item items
    putStrLn "Item added.\n"
    putStrLn "TODO Items:"
    putStrLn (displayItems newItems)
    pure newItems

main :: IO ()
main = do
    putStrLn "TODO app"
    let initialList = []
    interactWithUser initialList
    putStrLn "Thanks for using this app."
