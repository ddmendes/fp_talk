module Main where

type Item = String
type Items = [Item]

data Command
    = Quit
    | Help
    | DisplayItems
    | AddItem String
    | Done Int

parseCommand :: String -> Either String Command
parseCommand line = case words line of
    ["h"] -> Right Help
    ["q"] -> Right Quit
    ["i"] -> Right DisplayItems
    ["d", idxStr] ->
        if all (\c -> c `elem` "0123456789") idxStr
            then Right (Done (read idxStr))
            else Left "Invalid index."
    "a" : "-" : item -> Right (AddItem (unwords item))
    _ -> Left "Unknown command."

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
removeItem :: Int -> Items -> Either String Items
removeItem reversedIndex allItems =
        impl (length allItems - reversedIndex) allItems
    where
        impl index items =
            case (index, items) of
                (0, item : rest) -> Right rest
                (n, []) -> Left "Index out of bounds."
                (n, item : rest) ->
                    case impl (n-1) rest of
                        Right newItems -> Right (item : newItems)
                        Left errMsg -> Left errMsg

help :: IO ()
help = do
    let helpMessage = "Commands\n\
                       \    i: show all items\n\
                       \    a - <item>: add todo item\n\
                       \    d <index>: mark item as done\n\
                       \    q: quit\n"
    putStrLn helpMessage


-- Takes a list of items
-- Interact with the user
interactWithUser :: Items -> IO ()
interactWithUser items = do
    line <- getLine
    case parseCommand line of
        Right Help -> do
            help
            interactWithUser items

        Right DisplayItems -> do
            putStrLn "TODO items:"
            putStrLn (displayItems items)
            interactWithUser items

        Right (AddItem item) -> do
            let newItems = addItem item items
            putStrLn "Item added.\n"
            interactWithUser newItems

        Right (Done index) -> do
            let result = removeItem index items
            case result of
                Left errMsg -> do
                    putStrLn ("Error: " ++ errMsg ++ "\n")
                    interactWithUser items
                Right newItems -> do
                    putStrLn "Item done.\n"
                    interactWithUser newItems

        Right Quit -> do
            putStrLn "Bye!"
            pure ()

        Left errMsg -> do
            putStrLn ("Error: " ++ errMsg)
            interactWithUser items

main :: IO ()
main = do
    putStrLn "TODO App"
    help
    let initialList = []
    interactWithUser initialList
