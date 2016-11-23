-- Informatics 1 - Functional Programming 
-- Tutorial 5
--
-- Due: the tutorial of week 7 (3/4 November)


import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )

-- Warmup exercises

-- The datatype 'Fruit'
data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Apple _ _) = False
isBloodOrange (Orange varity _)= (varity == "Tarocco")||(varity == "Moro")||(varity == "Sanguinello")

-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments lF = sum [getsegments fruit | fruit <- lF, isBloodOrange fruit]
getsegments :: Fruit -> Int
getsegments (Apple _ _) = error "only work for orange"
getsegments (Orange _ seg) = seg
-- 3.
worms :: [Fruit] -> Int
worms lf = sum [1 |fruit <- lf,containworm fruit]
containworm :: Fruit -> Bool
containworm (Orange _ _) = False 
containworm (Apple _ hasWorm) = hasWorm

-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  (not (eval e p)) || eval e q
eval e (p :<->: q)    =  eval e p == eval e q

-- retrieves the names of variables from a proposition - 
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- Exercises ------------------------------------------------------------

-- 4.
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
p2 = (Var "P" :|: Var "Q") :&: ((Not (Var "P")) :&: (Not (Var "Q")))
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: (((Not (Var "P")) :|: (Not (Var "Q"))) :&: (Not (Var "P") :|: (Not (Var "R"))))


-- 5. 
tautology :: Prop -> Bool
tautology p = and [ eval e p | e <- envs (names p) ]

prop_taut1 :: Prop -> Bool
prop_taut1 p = tautology p || satisfiable (Not p)

prop_taut2 :: Prop -> Bool
prop_taut2 p = (not (satisfiable p)) || (not (tautology (Not p)))


-- 6.
p4 = (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q")
p5 = (Var "P" :->: Var "Q") :&: (Var "P" :&: (Not (Var "Q")))
p6 = (Var "P" :<->: Var "Q") :&: (( Var "P" :&:(Not (Var "Q"))) :&: ((Not (Var "P")) :&: Var "Q"))


-- 7.
equivalent :: Prop -> Prop -> Bool
equivalent p1 p2 = and [ (eval e p1 == eval e p2) | e <- envs (names (p1 :&: p2)) ]

equivalent' :: Prop -> Prop -> Bool
equivalent' p1 p2 = tautology (p1 :<->: p2)

prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent p1 p2= equivalent p1 p2 == equivalent' p1 p2


-- 8.
subformulas :: Prop -> [Prop]
subformulas p |(length lsub == 1) && ((head lsub == p)||((head (sub (head lsub))) == head lsub)) = [p]
              |(length lsub == 1) = [p] ++ subformulas (head lsub)
              | otherwise = [p] ++ (subformulas (head lsub) ++ subformulas (last lsub))
                 where lsub = sub p

sub :: Prop -> [Prop]
sub (Var x)        =  [Var x]
sub (F)            =  [F]
sub (T)            =  [T]
sub (Not p)        =  [p]
sub (p :|: q)      =  [p,q]
sub (p :&: q)      =  [p,q]
sub (p :->: q)     =  [p,q]
sub (p :<->: q)    =  [p,q]


-- Optional Material

-- 9.
-- check for negation normal form
isNNF :: Prop -> Bool
isNNF (p :|: q) = (isNNF p) && (isNNF q)
isNNF (p :&: q) = (isNNF p) && (isNNF q)
isNNF (Var x) = True
isNNF T = True
isNNF F = True
isNNF (Not (Var x)) = True
isNNF p = False

-- 10.
-- convert to negation normal form
toNNF :: Prop -> Prop
toNNF (p :|: q) = (toNNF p) :|: (toNNF q)
toNNF (p :&: q) = (toNNF p) :&: (toNNF q)
toNNF (p :->: q) = toNNF ((Not  p) :|:  q)
toNNF (p :<->: q) = toNNF ((p :->: q) :&: (q :->: p))
toNNF (Not (p :|: q)) = toNNF ((Not p) :&: (Not q))
toNNF (Not (p :&: q)) = toNNF ((Not p) :|: (Not q))
toNNF (Not (Not p)) = toNNF p
toNNF (Not (Var x)) = Not (Var x)
toNNF (Not T) = F
toNNF (Not F) = T
toNNF (Not p) = toNNF (Not (toNNF p))
toNNF p = p


-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Prop -> Bool
prop_NNF1 p  =  isNNF (toNNF p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Prop -> Bool
prop_NNF2 p  =  equivalent p (toNNF p)


-- 11.
-- check whether a formula is in conj. normal form
isCNF :: Prop -> Bool
isCNF (p :&: q) = (isCNF p) && (isCNF q)
isCNF (p :|: q) = (isDisj p) && (isDisj q)
isCNF (Var x) = True
isCNF (Not (Var x)) = True
isCNF p = False

isDisj :: Prop -> Bool
isDisj (p :|: q) = (isDisj p) && (isDisj q)
isDisj (Var x) = True
isDisj (Not (Var x)) = True
isDisj p = False

-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Prop]] -> Prop
listsToCNF [] = T
listsToCNF [[]] = F
listsToCNF [lp] =(listToDisj lp)
listsToCNF (lp:llp) | containEmpty (lp:llp) = F
                    | otherwise = ((listToDisj lp) :&: (listsToCNF llp))

containEmpty :: [[Prop]] -> Bool
containEmpty [] = False
containEmpty ([]:l) = True
containEmpty (p:l) = containEmpty l

listToDisj :: [Prop] -> Prop
listToDisj [p] = p
listToDisj (p:lp) = p :|: (listToDisj lp)

-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Prop -> [[Prop]]
listsFromCNF (p :&: q) = (listsFromCNF p) ++ (listsFromCNF q)
listsFromCNF (p :|: q) = [(listFromDisj p) ++ (listFromDisj q)]
listsFromCNF (Var x) = [[Var x]]
listsFormCNF (Not (Var x)) = [[Not (Var x)]]
listsFormCNF p = error "Not in right conjunctive form"

listFromDisj :: Prop -> [Prop]
listFromDisj (p :|: q) = (listFromDisj p) ++ (listFromDisj q)
listFromDisj (Var x) = [Var x]
listFromDisj (Not (Var x)) = [Not (Var x)]
listFormDisj p = error "Not in right disjuctive form"

-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Prop -> [[Prop]]
toCNFList p1 = toList (toNNF p1)
toList :: Prop -> [[Prop]]
toList (p :&: q) = toList p ++ toList q
toList (T :|: p) = []
toList (p :|: T) = []
toList (F :|: p) = toList p
toList (p :|: F) = toList p
toList ((a :&: b) :|: (c :&: d)) = toList(a :|: c) ++ (toList (a :|: d) ++ (toList(b :|: c) ++ toList(b :|: d)))
toList ((a :&: b) :|: c) = toList (a :|: c) ++ toList (b :|: c)
toList (a :|: (b :&: c)) = toList (a :|: b) ++ toList (a :|: c)
toList (p :|: q) = hp (toList p) (toList q)
toList (Var x) = [[Var x]]
toList (Not (Var x)) = [[Not (Var x)]]
toList F = [[]]
toList T = []

hp :: [[Prop]] -> [[Prop]] -> [[Prop]]
hp [] p2 = []
hp (l1:p1) p2 = [l1 ++ l2 | l2 <- p2] ++ (hp p1 p2)

-- convert to conjunctive normal form
toCNF :: Prop -> Prop
toCNF p  =  listsToCNF (toCNFList p)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Prop -> Bool
prop_CNF p  =  equivalent p (toCNF p)




-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"
