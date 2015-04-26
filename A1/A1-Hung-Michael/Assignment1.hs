{- 
 - CPSC 449 ASSIGNMENT ONE 
 - 24/09/2014
 - Submitted by Michael Y. Hung  (UCID: 10099049) 
 -}

module Assignment1 where
import Prelude hiding (min, gcd, elem)

 -- Problem 1 (a) --
 
 
 -- min takes two Integer arguments, compares them, and returns the smallest of the two.
 
min :: Integer -> Integer -> Integer
min x y
 |x <= y = x
 |otherwise = y

 -- minThree takes three Integer arguments, compares them, and returns the smallest of the three.
minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z
 |(x <= y) && (x <= z) = x
 |(y <= x) && (y <= z) = y 
 |(z <= x) && (z <= y) = z
 
 -- Problem 1 (b) --
 -- also included in pdf
  
{-

min (2 - 1) (1 - 2)
  ?? (2 - 1) <= (1 - 2)
  ?? ~> 1 <= -1 
  ?? ~> False
  ?? ~> -1
 
minThree (3+3) (2+2) (1+1)
  ?? ((3 + 3) <= (2 + 2)) && ((3 + 3) <= (1 + 1))
  ?? ~> (6 <= 4) && (6 <= 2)
  ?? ~> False && False
  ?? ~> False
  ?? ((2 + 2) <= (3 + 3)) && ((2 + 2) <= (1 + 1))  
  ?? ~> (4 <= 6) && (4 <= 2)
  ?? ~> False && False
  ?? ~> False
  ?? ((1 + 1) <= (3 + 3)) && ((1 + 1) <= (2 + 2))
  ?? ~> (2 <= 6) && (2 <= 4)
  ?? ~> True && True
  ?? ~> True
  ?? ~> 2
  
-} 
 
 -- Problem 2 --

 -- gcd takes two Integer arguments (both assumed to be positive), calculates their Greatest 
 -- Common Divisor using the Euclidean Algorithm, and returns it.
gcd :: Integer -> Integer -> Integer
gcd x y
 |y == 0 = x
 |otherwise = gcd y (x `mod` y)
 
 -- Problem 3 --
 
 -- matches takes two arguments: an Integer, and a list of Integers.
 -- A list of Integers is returned. This list contains each occurrence of the Integer provided
 -- in the list provided. For example, "matches 1 [1, 2, 3, 1]" will return "[1, 1]".
matches :: Integer -> [Integer] -> [Integer]
matches x y = [n | n <- y, n == x]

 -- elem takes an Integer and a list of Integers and evaluates whether or not the given Integer
 -- is an element of the given list of Integers. Returns True if it is, False if it isn't.
elem :: Integer -> [Integer] -> Bool
elem x y
 | length (matches x y) > 0 = True
 | otherwise = False

 -- Problem 4 --
 
type Person = String
type Book = String
type Database = [(Person, [Book])]

 -- An example database used for testing the functions.
exampleBase :: Database
exampleBase = [("Alice", ["Tintin", "Asterisk"]), ("Anna", ["Little Women"]), ("Rory", ["Tintin"])]

 -- Takes two arguments, the first of type Database and the second of type Person.
 -- books returns the list of Books that the indicated Person currently has loaned out according
 -- to the provided database. If the Person does not exist in the Database, an empty list is returned.
books :: Database -> Person -> [Book]
books d p = head [list |(person, list) <- d, person == p]

 -- Takes two arguments, the first of type Database and the second of type Book.
 -- borrowers returns the list of Persons that are currently loaning out the indicated Book
 -- according to the provided Database. If the book has not been loaned out by any Person, 
 -- an empty list is returned.
 -- notElem is used because the redefinition of elem used in Problem 3 does not account for
 -- Character lists.
borrowers :: Database -> Book -> [Person]
borrowers d b = [person | (person, books) <- d, not (b `notElem` books)]
 
 -- Takes two arguments, the first of type Database and the second of type Book.
 -- borrowed returns True/False depending on whether or not an indicated Book 
 -- has been loaned out according to the provided Database.
 -- notElem is used because the redefinition of elem used in Problem 3 does not account for
 -- Character lists.
borrowed :: Database -> Book -> Bool
borrowed d b 
 | length [ b | (person, books) <- d, not (b `notElem` books)] == 0 = False
 | otherwise = True

 -- Takes two arguments, the first of type Database and the second of type Person.
 -- numBorrowed returns the number of books that a given Person has loaned out
 -- according to the provided Database. A negative number should never be returned.
numBorrowed :: Database -> Person -> Int
numBorrowed d p = length (head [books | (person, books) <- d, person == p])

 -- Takes three arguments: a Database, a Person, and a Book.
 -- makeLoan updates the Database with a new loan - that is, a Person has loaned out a Book.
 -- If the Person is taking out their first Book, this new Person is added to the Database
 -- along with their new list of Books. If the Person still has outstanding Books, the new
 -- Book is simply added to their list.
 -- This function does not overwrite the original Database.
makeLoan :: Database -> Person -> Book -> Database
makeLoan d p b
 | length [person | (person, books) <- d, person == p] == 0 = [(p, [b])] ++ d 
 | otherwise = [(person, books ++ [b]) | (person, books) <- d, person == p] ++ [(person, books) | (person, books) <- d, person /= p] 

 -- Takes three arguments: a Database, a Person, and a Book.
 -- returnLoan updates the Database. If the Book that is to be returned is the only Book
 -- that the Person had loaned out, then the Person is removed from the Database entirely.
 -- if the Person still has more Books that are being loaned out, only the indicated Book
 -- is removed from the Database.
 -- This function does not overwrite the original Database.
returnLoan :: Database -> Person -> Book -> Database
returnLoan d p b
 | length (head [books | (person, books) <- d, person == p]) == 1 = [(person, books) | (person, books) <- d, person /= p]
 | otherwise = [(person, books) | (person, books) <- d, person /= p] ++ [(person, [book | book <- books, book /= b]) | (person, books) <- d, person == p]

 -- Problem 5 --
 
type Picture = [[Char]]

 -- exPic is an example Picture used for testing purposes.
exPic :: Picture
exPic = [['#','.','#'], ['.','.','#']]

 -- scale is the master function that takes a Picture argument and an Integer argument.
 -- It uses several helper functions to scale the provided picture by the given Integer.
 -- For example, "scale exPic 2" should return :
 -- ["##..##", "##..##", "....##", "....##"]
 -- Or, if we used scalePrint:
{-
   scale     #.#      2                       ##..##
             ..#                returns       ##..##
                                              ....## 
                                              ....##
 -}
scale :: Picture -> Integer -> Picture
scale [] _ = []
scale _ 0 = []
scale (p:ps) a
 | a < 0 = []
 | otherwise = repeatList (scaleHelper p a) a ++ scale ps a

 -- repeatList takes a list of Characters and an Integer as arguments.
 -- It returns a list of Character lists.
 -- repeatList takes the list of Characters and copies it the indicated
 -- number of times. For example, "repeatList ['a','b','c'] 2" will return 
 -- ["abc","abc"]. The purpose of this function is to scale the picture vertically.
repeatList :: [Char] -> Integer -> [[Char]]
repeatList _ 0 = []
repeatList x y = [x] ++ repeatList x (y - 1) 

 -- scaleHelper takes a list of Characters and an Integer as arguments.
 -- It returns a list of Characters.
 -- scaleHelper is an in-between function that helps to horizontally scale 
 -- each line of the Picture by applying repeatChar to each Character in the list.
 -- For example, "scaleHelper "abc" 2" will return "aabbcc"
scaleHelper :: [Char] -> Integer -> [Char]
scaleHelper [] _ = []
scaleHelper _ 0 = []
scaleHelper (x:xs) a = repeatChar x a ++ scaleHelper xs a

 -- repeatChar takes a Character and Integer as arguments and returns a list of Characters.
 -- repeatChar just copies a given Character the indicated number of times.
 -- For example, "repeatChar 'a' 2" will return "aa".
repeatChar :: Char -> Integer -> [Char]
repeatChar _ 0 = []
repeatChar x y = [x] ++ repeatChar x (y - 1)

 -- scalePrint is a complimentary function that prints the result of scale in picture form
 -- instead of as a list of Strings. It takes a Picture and an Integer to scale the Picture by.
 -- It traverses the Picture (a [[Char]]) and prints each element on a new line.
 -- This is an IO function so a dummy tuple is returned.
scalePrint :: Picture -> Integer -> IO ()
scalePrint x y = mapM_ putStrLn (scale x y)