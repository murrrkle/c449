{- 
 - CPSC 449 ASSIGNMENT TWO
 - 14/10/2014
 - Submitted by Michael Y. Hung  (UCID: 10099049) 
 -}

module Assignment2 where

 -- Problem 1 (a) --
 
 -- squareOdd takes a list of Integers as an argument and returns a list of all the squares of each
 -- Integer in the argument list that are odd. Uses list comprehension.
squareOdd :: [Integer] -> [Integer]
squareOdd xs = [x * x| x <- xs, not(x `mod` 2 == 0)]

 -- Problem 1 (b) --
 -- Does the same thing as squareOdd except using recursion instead of list comprehension.
squareOddRec :: [Integer] -> [Integer]
squareOddRec [] = []
squareOddRec (x:xs)
 | x `mod` 2 /= 0 = [x * x] ++ squareOddRec xs
 | otherwise      = squareOddRec xs

 -- Problem 2 (a) --
 -- parseHex takes a String of hexadecimal values and returns its decimal equivalent as an Integer.
parseHex :: String -> Integer
parseHex hexadec 
 | length hexadec /= 0 = charToInt (head (reverse hexadec)) + 16 * parseHex (reverse (removeTail (reverse hexadec)))
 | otherwise           = 0  
        where
            removeTail :: String -> String
            removeTail [] = []
            removeTail (_:hs) = hs
            charToInt :: Char -> Integer
            charToInt c
             | c == '0'  = 0
             | c == '1'  = 1
             | c == '2'  = 2
             | c == '3'  = 3
             | c == '4'  = 4
             | c == '5'  = 5
             | c == '6'  = 6
             | c == '7'  = 7
             | c == '8'  = 8
             | c == '9'  = 9
             | c == 'A'  = 10
             | c == 'B'  = 11
             | c == 'C'  = 12
             | c == 'D'  = 13
             | c == 'E'  = 14
             | c == 'F'  = 15
             | otherwise = 0
             
 -- Problem 2 (b) --
 -- encodeHex takes a decimal Integer and converts it into a hexadecimal, output as a String.
encodeHex :: Integer -> String
encodeHex n
 | (n `div` 16) == 0 = intToHex (n `mod` 16) 
 | otherwise = encodeHex (n `div` 16) ++ intToHex (n `mod` 16)
        where
            intToHex :: Integer -> String
            intToHex n
             | n == 0    = "0"
             | n == 1    = "1"
             | n == 2    = "2"
             | n == 3    = "3"
             | n == 4    = "4"
             | n == 5    = "5"
             | n == 6    = "6"
             | n == 7    = "7"
             | n == 8    = "8"
             | n == 9    = "9"
             | n == 10   = "A"
             | n == 11   = "B"
             | n == 12   = "C"
             | n == 13   = "D"
             | n == 14   = "E"
             | n == 15   = "F"
             | otherwise = "0"

 -- Problem 3 (a) --
 -- mergeLists takes two Integer lists and puts combines them. The argument lists are assumed to
 -- be sorted in an ascending fashion. By this assumption, the resulting list is also sorted in
 -- ascending order by mergeLists. This only holds true when the assumption is true.  
mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists [] z  = z
mergeLists z []  = z
mergeLists [] [] = []
mergeLists (x:xs) (y:ys)
 | x < y     =  [x] ++ mergeLists xs (y:ys)
 | otherwise = [y] ++ mergeLists (x:xs) ys 

 -- Problem 3 (b) --

 -- splitList takes an Integer list and divides it into two, where one list contains all 
 -- even-indexed elements and the other contains all odd-indexed elements.
splitList :: [Integer] -> ([Integer], [Integer])
splitList [] = ([], [])
splitList list = (getEvenElements list (length list), getOddElements list (length list))
        where
            getEvenElements :: [Integer] -> Int -> [Integer]
            getEvenElements [] _ = []
            getEvenElements list n
             | (n - 1) `mod` 2 == 0 = getEvenElements (init list) (n - 1) ++ [list !! (n - 1)]
             | otherwise = getEvenElements (init list) (n - 1)
            getOddElements :: [Integer] -> Int -> [Integer]
            getOddElements [] _ = []
            getOddElements list n
             | (n - 1) `mod` 2 /= 0 = getOddElements (init list) (n - 1) ++ [list !! (n - 1)]
             | otherwise = getOddElements (init list) (n - 1)
             
 -- Problem 3 (c) --

 -- mSort is an implementation of the merge sort algorithm that takes an Integer list and sorts it,
 -- returning another Integer list. This is accomplished through use of the previous two functions.
mSort :: [Integer] -> [Integer]
mSort [] = []
mSort [n] = [n]
mSort list
 | length list > 1 = mergeLists (mSort evenIndex) (mSort oddIndex)
 | otherwise = list
        where
            (evenIndex, oddIndex) = splitList list
            