module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined, Num (negate) )
import Distribution.Simple.Setup (trueArg)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero (S _) = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = zero
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O
even n = if even (pred n) == O 
  then S O
  else O

odd :: Nat -> Nat
odd O = O
odd n = if odd (pred n) == S O 
  then O
  else S O

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (-*)

(-*) :: Nat -> Nat -> Nat
O -* n = O
n -* O = n
S n -* S m = n -* m

infixl 6 -*

-- multiplication
(*) :: Nat -> Nat -> Nat
n * O = zero
S n * m = m * n + m
-- syntactic associativity: L
-- syntactic precedence: 7

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
n ^ O = one
n ^ (S m) = (n ^ m) * n

infixl 8 ^


-- quotient
(/) :: Nat -> Nat -> Nat
_ / O = undefined 
O / _ = zero
n / S m = 
  case n -* m of
    O -> O
    _ -> S ((n -* S m) / S m)

infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
n % O = undefined
n % S m = 
  case n -* m of
    O -> O
    S _ -> n -* (S m * (n / S m))
  

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
O ||| _ = O
(S n) ||| O = S O
(S m) ||| n = 
  case n % S m of
    O -> case S m -* n of
      O -> S O
      S _ -> O
    S _ -> O


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff O O = zero
absDiff (S n) O = S n
absDiff O (S n) = S n
absDiff (S n) (S m) = 
  case n -* m of
    O -> m -* n
    S _ -> n -* m  
  

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff


factorial :: Nat -> Nat
factorial O = one
factorial (S O) = one
factorial (S n) = S n * factorial n 


-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg (S n) = one

-- lo b a is the floor of the logarithm base b of a;
lo :: Nat -> Nat -> Nat 
lo _ O = undefined
lo (S O) (S (S _)) = undefined
lo O (S _) = undefined
lo _ (S O) = zero
lo (S (S m))  n = 
  case n / S (S m) of
    O -> O
    S _ -> S (lo (S (S m)) (n / S (S O)))