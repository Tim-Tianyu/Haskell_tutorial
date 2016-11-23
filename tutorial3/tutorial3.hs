-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 19-21 Oct.

import Data.Char
import Data.List
import Test.QuickCheck
import System.Random


-- 1. Map
-- a. 
uppers :: String -> String
uppers word = map toUpper word 

-- b.
doubles :: [Int] -> [Int]
doubles numL= map (2*) numL

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds prices = map (/100) (map fromIntegral prices)

-- d.
uppers' :: String -> String
uppers' word = [toUpper chrac| chrac <- word]

prop_uppers :: String -> Bool
prop_uppers word = uppers word == uppers' word



-- 2. Filter
-- a.
alphas :: String -> String
alphas word = filter isAlpha word 

-- b.
rmChar ::  Char -> String -> String
rmChar charc word= filter (charc /=) word

-- c.
above :: Int -> [Int] -> [Int]
above num numL = filter (num <=) numL

-- d.
-- can't do this without helpfunction
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals tupleL = filter unequal tupleL
unequal :: (Int,Int) -> Bool
unequal (x,y) = x /= y

-- e.
rmCharComp :: Char -> String -> String
rmCharComp charc word = [x | x <- word, charc /= x]

prop_rmChar :: Char -> String -> Bool
prop_rmChar charc word = (rmChar charc word == rmCharComp charc word)



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' word = map toUpper (filter isAlpha word)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' numL = map (2*) (filter (3<) numL)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strL = map reverse (filter lenEven strL)

lenEven :: String -> Bool
lenEven str = even(length str)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold numL = foldr (*) 1 numL

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (b:boolL) = b && andRec boolL

andFold :: [Bool] -> Bool
andFold boolL = foldr (&&) True boolL

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (l:listL) = l ++ concatRec listL

concatFold :: [[a]] -> [a]
concatFold listL = foldr (++) [] listL

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec "" str2 = str2
rmCharsRec (c:str1) str2 = c `rmChar` (rmCharsRec str1 str2)
--compeletely different
--rmCharsRec (c:str1) str2 = rmCharsRec str1 (rmChar c str2)

rmCharsFold :: String -> String -> String
rmCharsFold str1 str2 = foldr rmChar str2 str1

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform (n:num) = all (n==) num

-- b.
valid :: Matrix -> Bool
valid matrix = uniform (map length matrix)

all' :: (a -> Bool) -> [a] -> Bool
all' f l = foldr (&&) True (map f l)

-- 6.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f l1 l2 = [f a b | (a,b) <- (zip l1 l2)]

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f l1 l2 = map (uncurry f) (zip l1 l2)

-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2 | length m1 == length m2 = zipWith plusR m1 m2

plusR :: Num a => [a] -> [a] -> [a]
plusR l1 l2 | length l1 == length l2 =  zipWith (+) l1 l2

-- 8.
timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2  = [[ dot v1 v2 |v2 <- m2']| v1 <- m1]
             where m2' = transpose m2
dot :: [Int] -> [Int] -> Int 
dot v1 v2 | length v1 == length v2 = sum(zipWith (*) v1 v2)

-- Optional material
-- 9.

type MatrixR = [[Rational]]

checkSquare :: [[a]] -> Bool
checkSquare m = sum [1| v <- m,length v == l] == l
                where l = length m

appendI :: Matrix -> Int -> Matrix
appendI [] n = []
appendI (v:m) n = (v ++ (vectorI (length v) n)) : (appendI m (n+1))

vectorI :: Int -> Int-> [Int]
vectorI len pos | len == 0 = []
                | pos == 1 = 1 : (vectorI (len-1) (pos-1))
                | otherwise = 0 : (vectorI (len-1) (pos-1))
                             
transferR :: Matrix -> MatrixR
transferR m = [[toRational n| n <-v ]| v <- m]


inverse :: Matrix -> MatrixR
inverse m | (checkSquare m) = removeI (rowOp (transferR (appendI m 1)) (length m)) (length m)
          | otherwise = error "not square matrix"

rowOp :: MatrixR -> Int -> MatrixR
rowOp m 0 = m
rowOp m l = rowOp (column m (length(m)-l)) (l-1)

column :: MatrixR -> Int -> MatrixR
column m l= reduceTo0 (changeTo1 m l) l (length(m))

changeTo1 :: MatrixR -> Int -> MatrixR
changeTo1 m l | num /= 0 = (replace m (map (/num) (m !! l)) l)
              | otherwise = (swap0 m l (length(m)-l))
             where num = ((m !! l) !! l)
              
replace :: MatrixR -> [Rational] -> Int -> MatrixR
replace (x:m) v 0 = v:m
replace (x:m) v l = x:(replace m v (l-1))

swap0 :: MatrixR -> Int -> Int-> MatrixR
swap0 m _ 0 = error "not invertible"
swap0 m pos num | (v !! count) /= 0 = (replace m v pos)
                | otherwise = swap0 m pos (num-1)
                 where  count = length(m) - num
                        v = m !! count
reduceTo0 :: MatrixR -> Int -> Int-> MatrixR
reduceTo0 m _ 0 = []
reduceTo0 m pos num1 |(pos == count) || (num == 0) = vn:(reduceTo0 m pos (num1-1))
                     |otherwise = (plusR vn (map (*(-num)) (m!!pos))):(reduceTo0 m pos (num1-1))
                      where count = length(m) - num1
                            vn = m !! count
                            num = vn !! pos

removeI :: MatrixR -> Int -> MatrixR
removeI [] n = [] 
removeI (v:m) n = (drop n v) : (removeI m n) 


randomM :: Int -> Int -> Matrix
randomM dim num = [take dim (randomRs (1,10) (mkStdGen x))|x<-[num..(num+dim-1)]]
