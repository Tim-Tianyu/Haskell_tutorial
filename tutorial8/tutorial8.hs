-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 10 - due: 24/25 Nov.

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
states (xs,_,_,_,_) = xs

alph   :: FSM q -> Alphabet
alph (_,xs,_,_,_) = xs

start  :: FSM q -> q
start (_,_,x,_,_) = x

final  :: FSM q -> [q]
final (_,_,_,xs,_) = xs

trans  :: FSM q -> [Transition q]
trans (_,_,_,_,xs) = xs


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm source symbol = [b | (a,tran,b) <- (trans fsm), a == source, tran == symbol]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts fsm str =  travel fsm str (start fsm)

travel :: (Eq q) => FSM q -> String -> q -> Bool
travel fsm [] state = elem state (final fsm)
travel fsm (c:str) state = or [travel fsm str nextState | nextState <- next]
                         where next = delta fsm state c

-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical l = (sort.nub) l


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta fsm l symbol= (canonical.concat) [delta fsm state symbol|state <- l]


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm lSuper = (canonical.concat) (lSuper:[[ ddelta fsm x sym | sym <- (alph fsm)]| x <- lSuper])


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm lSuper | lSuper == nextSuper = lSuper
                     | otherwise = reachable fsm nextSuper
                     where nextSuper = next fsm lSuper


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm lSuper = (canonical.concat) [[x|f <- finals, elem f x ]| x <- lSuper]
                  where finals = final fsm

-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm lSuper = concat [[ (state, symbol,(ddelta fsm state symbol)) |symbol <- sym] |state <- lSuper]
                  where sym = alph fsm

-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic nfsm = (states, (alph nfsm), startL, dfinal nfsm states, dtrans nfsm states)
                   where startL = [start nfsm]
                         states = reachable nfsm [startL]

-- Optional Material
--11.
charFSM :: Char -> FSM Int
charFSM char = ([0,1], ['a'..'z'], 0, [1], [(0,char,1)])

emptyFSM :: FSM Int
emptyFSM = ([0], ['a'..'z'], 0, [0],[])

--12
intFSM :: (Ord q) => FSM q -> FSM Int
intFSM fsm = (stateInt, alph fsm, (find.start) fsm, [find x |x <- final fsm], [(find a,tran,find b) | (a,tran,b) <- trans fsm])
                  where
                  state = states fsm
                  stateInt = [0..(length state - 1)]
                  dic = zip state stateInt
                  find x = head [sn |(fs,sn) <- dic,  x == fs]

concatFSM :: Ord q => Ord q' => FSM q -> FSM q' -> FSM Int
concatFSM fsm1 fsm2 = concatINT (intFSM fsm1) (intFSM fsm2)

concatINT :: FSM Int -> FSM Int -> FSM Int
concatINT fsm1 fsm2 = (state1++state2',
                       canonical (alph fsm1 ++ alph fsm2), 
                       start fsm1, 
                       finalF,
                       trans fsm1 ++ trans2')
                    where 
                    state1 = states fsm1
                    len1 = length state1
                    len2 = length state2
                    start2 = start fsm2
                    final1 = (final fsm1)
                    state2 = [x | x <- states fsm2, x /= start2]
                    state2' = [len1..(len1+len2-1)]
                    dic = zip state2 state2'
                    find x = head [sn |(fs,sn) <- dic,  x == fs]
                    finalF = (canonical.concat) [if x == start2 
                                                 then [f | f <- final1] 
                                                 else [find x] 
                                                |x <- final fsm2]
                    trans2'= (canonical.concat) [if a == start2 
                                                 then concat [if b == start2 
                                                              then [(f1,t,f2) | f2 <- final1]
                                                              else [(f1,t,find b)]
                                                             |f1 <- final1]
                                                 else (if b == start2
                                                       then [(find a,t,f2)|f2 <- final1]
                                                       else [(find a,t,find b)])
                                                |(a,t,b) <- trans fsm2]
--13
stringFSM :: String -> FSM Int
stringFSM "" = emptyFSM
stringFSM (c:str) = concatFSM (charFSM c) (stringFSM str)


-- For quickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

prop_stringFSM1 n = accepts (stringFSM n') n'
      where n' = safeString n
prop_stringFSM2 n m = (m' == n') || (not $ accepts (stringFSM n') m')
                where m' = safeString m
                      n' = safeString n

--14
completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM (state',alph',start',final',trans')=(map Just state' ++ [Nothing], alph', Just start', map Just final', tran)
                         where
                         dic = canonical [(a,t) | (a,t,b) <- trans']
                         tranN= concat [[(Just x, symbol,Nothing)
                                        |symbol <- alph',not (elem (x,symbol) dic)]
                                       | x <- state']
                         tran = sort ([(Just a,t,Just b)|(a,t,b) <- trans'] ++ tranN ++ nothing)
                         nothing = [(Nothing,sym,Nothing) | sym <- alph']

unionFSM :: (Ord q) => FSM q -> FSM q -> FSM Int
unionFSM fsm1 fsm2 = (stateU, symU, startU, finalU, transU)
                   where (state1,sym1,start1,final1,trans1) = intFSM fsm1
                         (state2,sym2,start2,final2,trans2) = intFSM fsm2
                         symU = canonical (sym1++sym2)
                         startU = start1
                         state2' = [x | x <- state2, x /= start2]
                         state2'' = [(length state1)..((length state1)+(length state2')-1)]
                         stateU = (state1 ++ state2'')
                         dic = zip state2' state2''
                         find x | length l > 0 = head l
                                | otherwise = startU
                                where l =[b |(a,b) <- dic, x==a]
                         finalU = canonical (final1 ++ map find final2)
                         trans' = [(find a, t, find b)|(a,t,b) <- trans2]
                         transU = canonical (trans1 ++ trans')
                         
                         
                         
        
prop_union n m l =  accepts (unionFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l'|| accepts (stringFSM m') l') &&
                    accepts (unionFSM (stringFSM n') (stringFSM m')) n' && accepts (unionFSM (stringFSM n') (stringFSM m')) m'
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l

--15
star :: (Ord q) => FSM q -> FSM q
star (states',alph',start',final',trans') = (statesS,alphS,startS,finalS,transS)
               where 
               statesS = [x | x <- states',x /= start']
               alphS = alph'
               startS = head final'
               finalS = final'
               transL = [if a==start' && b== start' 
                         then [(a',t,b')|a' <- final', b' <- final']
                         else if a == start'
                              then [(a',t,b)|a' <- final']
                              else if b == start'
                                   then [(a,t,b')| b' <- final']
                                   else [(a,t,b)]
                        | (a,t,b) <- trans']
               transS = concat transL

    
prop_star a n = (star $ stringFSM a') `accepts` (concat [a' | x <- [0..n]]) &&
                (star $ stringFSM a') `accepts` ""
      where a' = safeString a

--16
complement :: (Ord q) => FSM q -> FSM Int
complement fsm = intFSM (statesC,alphC,startC,finalC,transC)
               where (states',alph',start',final',trans') = completeFSM fsm
                     statesC = states'
                     alphC = alph'
                     startC = start'
                     finalC = [x | x <- states', not (elem x final')]
                     transC = trans'

prop_complement :: String -> String -> Bool
prop_complement n m = (n' == m')
                      || accepts (complement $ stringFSM n') m'
                      && (not $ accepts (complement $ stringFSM n') n)
                      where n' = safeString n
                            m' = safeString m

-- 17.
intersectFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
intersectFSM (state1,sym1,start1,final1,trans1) (state2,sym2,start2,final2,trans2) 
             = (stateI,symI,startI,finalI,transI)
             where
             symI = canonical (sym1 ++ sym2)
             startI = (start1, start2)
             finalI = [(x,y)| x <- final1, y <- final2, elem (x,y) stateI]
             
             delta' :: (Ord q) => [Transition q] -> q -> Char -> [q]
             delta' tran source sym = [b | (a,t,b) <- tran, a == source, t == sym]

             travel :: (Ord q) => [Transition q] -> [Transition q] -> (q,q) -> Char -> [(q,q)]
             travel t1 t2 (a1,a2) sym = [(b1,b2)| b1 <- delta' t1 a1 sym, b2 <- delta' t2 a2 sym]

             next :: (Ord q) =>  [Transition q] -> [Transition q] -> (q,q) -> [(q,q)]
             next t1 t2 (a1,a2) = (canonical.concat) ([(a1,a2)]:[travel t1 t2 (a1,a2) sym | sym <- symI])

             reach :: (Ord q) =>  [Transition q] -> [Transition q] -> [(q,q)] -> [(q,q)]
             reach t1 t2 state | state == nextState = state
                               | otherwise = reach t1 t2 nextState
                         where nextState = (canonical.concat) [next t1 t2 x | x <- state]

             stateI = reach trans1 trans2 [startI]
             transI = (canonical.concat) [[(a,t,b) |b <- travel trans1 trans2 a t]| a <- stateI, t <- symI]
             
             
                
prop_intersect n m l = accepts (intersectFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l' && accepts (stringFSM m') l')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l



prop1 a b = star ((stringFSM a') `unionFSM` (stringFSM b')) `accepts` (a'++b'++a'++a')
 where a' = safeString a
       b' = safeString b

prop2 a b = ((stringFSM a') `intersectFSM` (intFSM ((stringFSM b') `unionFSM` (stringFSM a')))) `accepts` a'
             where a' = safeString a
                   b' = safeString b


