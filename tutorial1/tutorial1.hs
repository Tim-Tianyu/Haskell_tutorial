-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (5-7th Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ div x 2 | x <- xs, mod x 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | even x = (div x 2) : halveEvensRec xs
                     | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = (halveEvens xs == halveEvensRec xs)



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, x >= lo && x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | (x >= lo && x <= hi) = x:inRangeRec lo hi xs
                        | otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = (inRange lo hi xs == inRangeRec lo hi xs)



-- 3. countPositives: count the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = sum([ 1 | x <- xs, x > 0 ])

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0 = 1 + countPositivesRec xs
                         | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositivesRec xs == countPositives xs



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round((fromIntegral x) * 0.9)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum([discount x | x <- xs, (discount x) <= 19900 ])

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) | discount x <= 19900 =  (discount x) + (pennypincherRec xs)
                       | otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = (pennypincherRec xs) == (pennypincher xs)



-- 5. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [digitToInt x | x <- xs,  isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (x:xs) | isDigit x = (digitToInt x) * (multDigitsRec xs)
                     | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = (multDigits xs == multDigitsRec xs)



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise "" = ""
capitalise(x:xs) = (toUpper x) : [ toLower a | a <- xs ]

-- Recursive version
lower "" = ""
lower (x:xs) = toLower x : lower xs
capitaliseRec :: String -> String
capitaliseRec "" = ""
capitaliseRec (x:xs) = toUpper x : lower xs

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = capitaliseRec xs == capitalise xs



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (x:xs) = capitalise x :[ check a | a <- xs ]
check :: String -> String
check x | length x > 3 = capitalise x
        | otherwise = lower2 x
lower2 :: String -> String
lower2 xs = [toLower x | x <- xs]

-- Recursive version
titleRechp :: [String] -> [String]
titleRechp [] = []
titleRechp (x:xs) | length x > 3 = capitalise x : titleRechp xs
                  | otherwise = lower2 x : titleRechp xs
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) = capitalise x : titleRechp xs

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [x | x<-words,pos >= 0, len > pos,length x == len, ((!!) x pos) == letter]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec letter pos len [] = []
crosswordFindRec letter pos len (word:words) |pos >= 0 && len>pos && length word == len
                                              && (!!) word pos==letter
                                                 = word: (crosswordFindRec letter pos len words)
                                             |otherwise
                                                 = (crosswordFindRec letter pos len words)

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind letter pos len words = 
                   crosswordFindRec letter pos len words == crosswordFind letter pos len words



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search word charc = [fst x | x <- (zip [0..length word -1] word), snd x == charc]

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec word charc = searchRechp word charc 0
searchRechp [] charc pos = []
searchRechp (c:word) charc pos | c == charc  = pos : searchRechp word charc (pos+1)
                               | otherwise = searchRechp word charc (pos+1) 


-- Mutual test
prop_search :: String -> Char -> Bool
prop_search word charc = search word charc == searchRec word charc


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains word sub = length [1| x<-[0..length word], isPrefixOf sub (drop x word)] > 0 

-- Recursive version
containsRec :: String -> String -> Bool
containsRec "" sub | length sub > 0 = False
                   | otherwise = True
containsRec word sub | isPrefixOf sub word = True
                     | otherwise = containsRec (drop 1 word) sub

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains word sub = contains word sub == containsRec word sub

