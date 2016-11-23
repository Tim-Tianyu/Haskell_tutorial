-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 12-14 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate num stri |num >= 0 && num <= length stri = drop num stri ++ take num stri
                |otherwise = [(!!) stri num] 

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey num = zip letters (rotate num letters) 
              where letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp charc key | length secd > 0 = head secd
                 | otherwise = charc
                 where secd = [snd x | x <- key, fst x == charc]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec charc [] = charc
lookUpRec charc (k:key) | fst k == charc = snd k
                        | otherwise = lookUpRec charc key

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp charc key = lookUp charc key == lookUpRec charc key

-- 5.
encipher :: Int -> Char -> Char
encipher num charc = lookUp charc (makeKey num)

-- 6.
normalize :: String -> String
normalize word = [toUpper charc | charc <- word, isAlpha charc || isDigit charc]

-- 7.
encipherStr :: Int -> String -> String
encipherStr num word = [encipher num charc | charc <- normalize word]
-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [(snd k, fst k) | k <- key]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (k:key) = (snd k, fst k) : reverseKeyRec key

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey key = reverseKey key == reverseKeyRec key
-- 9.
decipher :: Int -> Char -> Char
decipher num charc =  lookUp charc (reverseKey (makeKey num))

decipherStr :: Int -> String -> String
decipherStr num word = [decipher num charc | charc <- normalize word]

-- 10.
contains :: String -> String -> Bool
contains word sub = sum [1| x<-[0..length word], isPrefixOf sub (drop x word)] > 0 

-- 11.
candidates :: String -> [(Int, String)]
candidates word = [(num,decipherStr num word) | num <- [0..25], (contains (decipherStr num word) "THE") || (contains (decipherStr num word) "AND")]



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive "" = []
splitEachFive word |length word > 4 = (take 5 word) : splitEachFive (drop 5 word)
                   |otherwise = [word ++ (replicate (5 -length word) 'X')]

-- 13.
prop_transpose :: String -> Bool
prop_transpose word = transpose (transpose (matrix)) == matrix
                      where matrix = splitEachFive word

-- 14.
encrypt :: Int -> String -> String
encrypt num word = glue(transpose (splitEachFive (encipherStr num word)))
glue :: [String] -> String
glue [] = []
glue (word:words) = word ++ glue words

-- 15.
decrypt :: Int -> String -> String
decrypt num word = decipherStr num (glue (transpose (split5 row word)))
                  where row = round ((fromIntegral (length word)) / 5)
split5 :: Int -> String -> [String]
split5 _ "" = []
split5 num word = take num word : (split5 num (drop num word))
    
-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs "" = []
countFreqs (c:word) = (c, (length word -length word' + 1)) : countFreqs word'
                       where word' = takeOut c word
takeOut :: Char -> String -> String
takeOut _ "" = ""
takeOut charc (c:word) | c == charc = takeOut charc word
                       | otherwise = c : takeOut charc word

-- 17
freqDecipher :: String -> [String] 
freqDecipher word = [decrypt (mod ((ord (fst x)) - 69 + 26) 26) word | x <- (sortBy sorthp (countFreqs word))]

sorthp :: (a,Int) -> (a,Int) -> Ordering
sorthp a b = compare (snd b) (snd a)
