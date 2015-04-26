{- 
 - CPSC 449 ASSIGNMENT FOUR
 - 26/10/2014
 - Submitted by Michael Y. Hung  (UCID: 10099049) 
 -}
 
module Assignment4 where

data Poly = PConst Int | PVar | PAdd Poly Poly | PMul Poly Poly

--compilePoly :: Poly -> (Int -> Int)
--compilePoly = \p -> p


{-
factors :: Int -> [Int]
factors n = take n (sieve [1..])
        where sieve (b:xs) = b : sieve [ x | x <- xs, (n `rem` x) == 0]
-}      

pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = [(a, b, c) | c <- [1..], a <- [1..c], b <- [1..a], a*a + b*b == c*c]