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
import System.Win32 (COORD(yPos), xBUTTON1)

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
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile b (x : xs) = if b x then x : takeWhile b xs else []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile b (x : xs) = if b x then dropWhile b xs else x : xs

-- tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

-- init
init :: [a] -> [a]
init [] = error "empty list"
init [_] = []
init (x : xs) = x : init xs

-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [[]] ++ map (x : ) (inits xs)


-- subsequences
subsequences :: Eq a => [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) = let rest = subsequences xs
                          in rest ++ map (x:) rest


-- any
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x : xs) = p x || any p xs

-- all
all :: (a -> Bool) -> [a] -> Bool
all _ [] = False 
all p (x : xs) = p x && all p xs

-- and
and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

-- or
or :: [Bool] -> Bool
or [] = False
or (x : xs) = x || or xs

-- concat
concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x xs = any (== x) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys) = case x == y of
  True -> True
  False -> elem' x ys 

-- (!!)
(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(_ : xs) !! i = xs !! (i - 1)
_ !! _ = error "index non existent"

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) = case p x of
  True -> x : filter p xs 
  False -> filter p xs
-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = error "empty list"
cycle xs = xs ++ cycle xs

-- repeat
repeat :: a -> [a]
repeat a = a : repeat a

-- replicate
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate i x = x : replicate (i - 1) x

-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = case x == y of
  True -> isPrefixOf xs ys
  False -> False

-- isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf (x : xs) (y : ys) = any (isPrefixOf (x : xs)) (tails (y : ys))

-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf (x : xs) (y : ys) = isPrefixOf (reverse (x : xs)) (reverse (y : ys))

-- zip
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x , y) : zip xs ys

-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

-- intercalate
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate y (x : xs) = x ++ y ++ intercalate y xs

-- nub
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)

-- splitAt
splitAt :: Int -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt 0 xs = ([], xs)
splitAt i xs = (take i xs, drop i xs)

-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
-- GUESS: the problem in this definition would be shown if n was negative?

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break p (x : xs) = case p x of
  True -> ([], x : xs)
  False -> (x : ys, zs)
  where 
    (ys, zs) = break p xs

-- lines
lines :: String -> [String]
lines "" = []
lines s =
  case break (=='\n') s of
    (line, "") -> [line]
    (line, _:rest) -> line : lines rest

-- words
words :: String -> [String]
words s =
  case dropWhile (== ' ') s of
    "" -> []
    s' -> case break (== ' ') s' of
      (word, rest) -> word : words rest

-- unlines
unlines :: [String] -> String
unlines [] = ""
unlines (x : xs) = x ++ "\n" ++ unlines xs

-- unwords
unwords :: [String] -> String
unwords [] = ""
unwords (x : xs) = x ++ " " ++ unwords xs

-- transpose
-- I spent a lot of time trying to think of something to put here, but, sadly, 
-- I wasn´t able to come up with anything :(

-- checks if the letters of a phrase form a palindrome (see below for examples)
-- function I will need for the palindrome definition
-- toLower
toLower :: Char -> Char
toLower c = case c >= 'A' && c <= 'Z' of
  True -> toEnum (fromEnum c + 32)
  False -> c

-- isLetter
isLetter :: Char -> Bool
isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

palindrome :: String -> Bool
palindrome s =
  let text = map toLower (filter isLetter s)
  in and (zipWith (==) text (reverse text))

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}


