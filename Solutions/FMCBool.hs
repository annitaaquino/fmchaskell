module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where
    show True = "True"
    show False = "False"

instance Enum Bool where
    toEnum 0 = False
    toEnum 1 = True

    fromEnum False = 0 
    fromEnum True = 1

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ False = False
(&&) False _ = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) _ True = True
(||) False False = False

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) True True = False
(/|\) _ False = True
(/|\) False _ = True


infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) True _ = False
(\|/) _ True = False
(\|/) False False = True

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) True False = True
(<=/=>) False True = True
(<=/=>) True True = False
(<=/=>) False False = False


infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x y = x
ifThenElse False x y = y

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) False _ = undefined
(==>) True True = True
(==>) True False = False
infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) _ False = False
(<==) True True = True
(<==) False True = False


infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) True True = True
(<=>) False False = True
(<=>) True False = False
(<=>) False True = False 

infixr 1 <=>


