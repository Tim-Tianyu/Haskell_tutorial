-- Informatics 1 Functional Programming
-- Tutorial 7
--
-- Due: 17/18 November

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum [length name | (_,(name,_)) <- xs]

formatLine :: Int -> (Barcode, Item) -> String
formatLine len (bcode,(name,unit)) | len >= lenName 
           = bcode ++ "..." ++ name ++ (take (3+len-lenName) (repeat '.')) ++ unit
                                   | otherwise
           = error "desired length too short"
           where lenName = length name

showCatalogue :: Catalogue -> String
showCatalogue catlog = concat [formatLine len x ++ "\n" | x <- list]
                     where
                     list = toList catlog
                     len = longestProductLen list


-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [a] = (Just a)
listToMaybe xs = error "Invalid List"

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [head (maybeToList x)| x <- xs , length (maybeToList x) /= 0]

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems l catlog = catMaybes [get x catlog | x <- l]






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
