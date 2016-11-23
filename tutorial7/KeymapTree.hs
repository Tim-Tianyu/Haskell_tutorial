-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth tree  = maximum (depthList tree 0)

depthList :: Ord k => Keymap k a -> Int -> [Int]
depthList Leaf depth = [depth]
depthList (Node _ _ left right) depth = (depthList left (depth+1)) ++ (depthList right (depth+1)) 

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k a left right) = (toList left) ++ [(k,a)] ++ (toList right)

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value tree = f key value tree
    where
      f key value Leaf = Node key value Leaf Leaf
      f key value (Node k v left right) | key == k = Node k value left right
                                        | key < k  = Node k v (f key value left) right 
                                        | key > k  = Node k v left (f key value right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key Leaf = Nothing
get key (Node k v left right) | key == k = Just v
                              | key < k  = get key left
                              | key > k  = get key right 

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList [] = Leaf
fromList ((k,a):list) = set k a (fromList list)



prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT num (Node k a left right) | num >= k  = Node k a left (filterLT num right) 
                                   | otherwise = (filterLT num left)

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT num (Node k a left right) | num <= k  = Node k a (filterGT num left) right
                                   | otherwise = (filterGT num right)

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf tree2 = tree2
merge (Node k a left right) tree2 = Node k a (merge left (filterLT k tree2)) (merge right (filterGT k tree2))

prop_merge :: [Int] -> [Int] -> [Int] -> Bool
prop_merge k a1 a2 = toList (merge (fromList l1) (fromList l2)) == sort (l1 ++ l2)
                      where
                      key = nub k
                      len = length key
                      l1 = zip (take (div len 2) key) a1
                      l2 = zip (drop (div len 2) key) a2

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del _ Leaf = Leaf
del key (Node k a left right) | key == k = merge right left
                              | key > k = Node k a left (del key right)
                              | key < k = Node k a (del key left) right

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select _ Leaf = Leaf
select f (Node k a left right) | f a       = Node k a (select f left) (select f right)
                               | otherwise = merge (select f left) (select f right)

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
