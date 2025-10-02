{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import System.Win32 (COORD(yPos))

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "empty list"
head (x : _) = x

tail :: [a] -> [a]
tail [] = error "empty list"
tail (_ : xs) = xs

null :: [a] -> Bool
null [] = True 
null (x : _) = False

length :: Integral i => [a] -> i
length [] = 0
length (x : xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs 

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x] 

(++) :: [a] -> [a] -> [a]
(++) [] xs = xs
(++) (x : xs) ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc a []= [a]
snoc a (x : xs) = (x : xs) ++ [a]

(<:) :: [a] -> a -> [a]
(<:) [] a = [a]
(<:) (x : xs) a = (x : xs) ++ [a]

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a
minimum [] = error "empty list"
minimum [a] = a
minimum (x : xs) = min x (minimum xs)

-- maximum :: Ord a => [a] -> a
maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [a] = a
maximum (x : xs) = max x (maximum xs)

-- take
take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take  i (x : xs) = x : take (i - 1) xs

-- drop
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop i [] = []
drop i (x : xs) = drop (i - 1) xs



-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
-- palindrome :: String -> Bool
-- palindrome = undefined


