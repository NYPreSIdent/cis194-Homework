-- Enumeration types
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
    deriving (Show)
-- The five items are the value of the type Thing.

isSmall :: Thing  -> Bool
isSmall Shoe       =  True
isSmall Ship       =  False
isSmall SealingWax =  True
isSmall Cabbage    =  True
isSmall King       =  False

-- We can reduce the code to:
isSmall2 :: Thing -> Bool
isSmall2 Ship     =  False
isSmall2 King     =  False
isSmall2 _        =  True

-- Enumeration type is only a spectial case of Haskell's more general algebraic data types.

data FailableDouble = Failure
                    | OK Double
                      deriving (Show)

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- data constructors can have more than one argument.
data Person = Person String Int Thing
  deriving (Show)

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Brent" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- "Type Constructor and Data(Value) Constructor may be the same,
--  in haskell, the names of types and values are independent of each other.
--  We use a type constructor in a type declaration or a type signature. We use value
--  constructor in actual code. If we are using a type signature, we must be referring 
--  to a type constructor. If we are writing an expression, we must be using the value constructor." 
--  --From << Real World Haskell>>

-- ADT in general.
-- An ADT has one or more data constructors, and each data constructor can hava more than zero arguments.
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4

-- Pattern-matching
-- Fundamentally, pattern-matching is about taking apart a value by finding out which constructor it was built with.
-- in Haskell, this is the only way to make a decision.
-- 1. We can give names to the values that come along with each constructors.
-- 2. Parentheses are required around patterns consisting of more than just a single constructor.
-- more things to notice:
-- 1. An underscore _ can be used as a "wildcard pattern" which matches anything.
-- 2. A pattern of the form a@pat can be used to match a value against the pattern pat, but also give the name x to 
-- the entire value being matched, for example:
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is" ++ n

-- result: *Mian> baz brent
-- "The name field of (Person \"Brent\" 31 SealingWax) is Brent.

-- 3. patterns can be nested. For examle:
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."
-- SealingWax is nested in the pattern.

-- The following grammar defines what can be used as a pattern.
pat :: = _ -- an underscore is a pattern.
       | var -- a variable it self is a pattern: such a pattern matched anything, and "binds" the given variable name to the mathched value.
       | var @ ( pat ) -- specifies @-patterns.
       | ( Constructor pat1 pat2 ... patn ) -- a constructor name followed by a sequence of patterns is itself a pattern.
-- Last line: such a pattern mathces a value if that value was constructed using the given constructor, and pat1 through patn all match the values
-- contained by the constructor, recursively.

-- You can think that literal values like 2 or "a" are constructors with no arguments.
-- Int can Char were defined as : 
data Int1 = 0 | 1 | -1 | 2 | ...
data Char1 = 'a' | 'b' | 'c' | 'd' ...
-- Of course, they are not actually defined this way.

-- Case expressions
-- The fundamental construct for doing pattern-matching in Haskell.
case exp of
	pat -> exp1
	pet -> exp2
	...

-- When evaluated, the expression exp is mathced against each of the patterns pat1, pat2, ... in turn.
-- The first matching pattern is chosen, and the entire case expression evaluates to the expression corresponding
-- to the matching pattern.
-- In fact, the syntax for defining functions we have seen is really just convenient syntax sugar for defining
-- a case expression. For example, the definition of failureToZero given previously can equavalently be writtern as:
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                       Failure -> 0
                       OK d    -> d

-- Recursive types: defined in terms of itself.
-- a list is recursive: A list is either empty, or a single element followd by a remaning list.
data IntList = Empty | Cons Int IntList

-- We often use recursive functions to process recursive data types.
intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

-- binary tree:
data Tree = Leaf Char
          | Node Tree Int Tree
            deriving (Show)
