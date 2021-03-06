-- Informatics 1 - Functional Programming 
-- Tutorial 6
--
-- Week 8 - Due: 10/11 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (p :#: q) = (split p) ++ (split q)
split Sit = []
split (Turn x) = [Turn x]
split (Go x) = [Go x]

-- 1b. join
join :: [Command] -> Command
join [cmd] = cmd
join (cmd:l)= cmd :#: (join l)

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent c1 c2 = (split c1) == (split c2)

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c

prop_split :: Command -> Bool
prop_split c = check (split c)

check :: [Command] -> Bool
check [] = True
check (Sit:l) = False
check ((p :#: q):l) = False
check (c:l) = check l

-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy num cmd = join (take (num*l) (cycle list))
             where list = split cmd
                   l = length list

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon dis = copy 5 (Go dis :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon dis side = copy side (Go dis :#: Turn angle)
                 where angle = (360 / (fromIntegral side))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral dis 1 step angle = Go dis :#: Turn angle
spiral dis num step angle | dis < ((fromIntegral num) * step) = spiral ((fromIntegral num) * step) num step angle
                          | otherwise = Go dis :#: (Turn angle :#: spiral (dis - step) (num - 1) step angle)


-- Exercise 4
-- optimise
--optimise :: Command -> Command
--optimise (Sit :#: c) = optimise c
--optimise (c :#: Sit) = optimise c
--optimise (Go 0.0 :#: c) = optimise c
--optimise (c :#: Go 0.0) = optimise c
--optimise (Turn 0.0 :#: c) = optimise c
--optimise (c :#: Turn 0.0) = optimise c
--optimise (c1 :#: c2) = combine ((optimise c1) :#: (optimise c2))
--optimise c = c

--combine :: Command -> Command
--combine (Turn a :#: Turn b) = (Turn (a+b))
--combine (Go a :#: Go b) = (Go (a+b))
--combine (Go 0.0 :#: c) = c
--combine (c :#: Go 0.0) = c
--combine (Turn 0.0 :#: c) = c
--combine (c :#: Turn 0.0) = c
--combine c = c

optimise :: Command -> Command
optimise c = join (combine (preOptimise (split c)))

preOptimise :: [Command] -> [Command]
preOptimise [] = []
preOptimise (Turn 0 : xs) = preOptimise xs
preOptimise (Go 0 : xs) = preOptimise xs
preOptimise (x : xs) = x : preOptimise xs

combine :: [Command] -> [Command]
combine [] = []
combine [Turn 0] = []
combine [Go 0] = []
combine [x] = [x]
combine (Turn a : Turn b : xs) = combine (Turn (a+b) : xs)
combine (Go a : Go b : xs) = combine (Go (a+b) : xs)
combine (Turn 0 : xs) = combine xs
combine (Go 0 : xs) = combine xs
combine (x:xs) = (x:combine xs)

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
      where
      f 0 = GrabPen red :#: Go 10
      f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
      g 0 = GrabPen blue :#: Go 10
      g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
      n   = Turn 60
      p   = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
      where
      f 0 = Go 10
      f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
      n   = Turn 60
      p   = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
    where
    l 0 = Sit
    l x = p :#: r (x-1) :#: f (x-1) :#: n :#: l (x-1) :#: f (x-1) :#: l (x-1) :#: n :#: f (x-1) :#: r (x-1) :#: p
    r 0 = Sit
    r x = n :#: l (x-1) :#: f (x-1) :#: p :#: r (x-1) :#: f (x-1) :#: r (x-1) :#: p :#: f (x-1) :#: l (x-1) :#: n
    f x = GrabPen green :#: Go 10
    n   = Turn 90
    p   = Turn (-90)

peano :: Int -> Command
peano x = f x
  where
  f 0 = GrabPen red :#: Go 10
  f x = f (x-1) :#: p :#: g (x-1) :#: p :#: p :#: g (x-1) :#: n :#: f (x-1) :#: n :#: n :#: f (x-1) :#: f (x-1) :#: n :#: g (x-1) :#: p
  g 0 = GrabPen blue :#: Go 10
  g x = n :#: f (x-1) :#: p :#: g (x-1) :#: g (x-1) :#: p :#: p :#: g (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: n :#: g (x-1)
  n   = Turn 60
  p   = Turn (-60)

cross :: Int -> Command
cross x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x :#: n
  where
  f 0 = Go 10
  f x = f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
  n   = Turn 90
  p   = Turn (-90)

branch :: Int -> Command
branch x = g x
   where
   g 0 = GrabPen blue :#: Go 10
   g x = f (x-1) :#: n :#: Branch (Branch (g (x-1)) :#: g (x-1)) :#: p :#: f (x-1) :#: Branch ( p :#: f (x-1) :#: g (x-1)) :#: n :#: g (x-1)
   f 0 = GrabPen red :#: Go 10
   f x = f(x-1) :#: f(x-1)
   n = Turn (22.5)
   p = Turn (-22.5)

segment :: Int -> Command
segment x = f x :#: p :#: f x :#: p :#:  f x :#: p :#: f x
    where
    f 0 = Go 10
    f x = n :#: f(x-1) :#: p :#: f(x-1) :#: n :#: f(x-1) :#: n :#: f(x-1) :#: p :#: f(x-1) :#: p :#: f(x-1) :#: f(x-1) :#: n :#: f(x-1) :#: p :#: f(x-1) :#: p :#: f(x-1) :#: f(x-1) :#: p :#: f(x-1) :#: n :#: f(x-1) :#: n :#: f(x-1) :#: f(x-1) :#: p :#: f(x-1) :#: f(x-1) :#: n :#: f(x-1) :#: f(x-1)  :#: p :#: f(x-1) :#: p :#: f(x-1) :#: n :#: f(x-1) :#: f(x-1) :#: n :#: f(x-1) :#: n :#: f(x-1) :#: p :#: f(x-1) :#: f(x-1) :#: n :#: f(x-1) :#: n :#: f(x-1) :#: p :#: f(x-1) :#: p :#: f(x-1) :#: n :#: f(x-1) :#: p
    n = Turn 90
    p = Turn (-90)
