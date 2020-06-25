-- Recursion patterns, polymorphism, and the Prelude.
-- In face, experienced Haskell programmers hardly ever write recursive functions.
-- Why? The typical recursive function patterns are abstrcted into the library, and we can think about problems
-- at a higher level rather than details.

-- The goal of wholemeal programming : leave the low-level details and think about problems at a higher level!

-- First Example:
data IntList = Empty | Cons Int IntList
    deriving (Show)

-- Some Common operations may on the IntList:
-- 1. Perform some operation on every element of the list. (map)
-- 2. Keep only some elements of the list, and throw others away, based on test (filter)
-- 3. "Summarize" the elements of the list somehow (sum, product, maximum)
-- 4. Using the structure to represent some control flows.

-- 1.map function
absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

-- or we can do:
squareAll :: IntList  -> IntList
squareAll Empty       -> Empty
squareAll (Cons x xs) =  Cons (x * x) (squareAll xs)

-- They are so simmilar to each other, and we can find a way to abstract out the commonality, so we don't have to 
-- repeat ourselves!
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

mapIntList addOne exampleList
mapIntList abs    exampleList
mapIntList sqaure exampleList

-- Filter, keep some elements of the list and throw others ways.
keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x    = Cons x (keepOnlyEven xs)
    | otherwise = keepOnlyEven xs

-- summarize the elements of the list. Flod: fold or reduce.

-- Polymorphism: Haskell supports polymorphism for both data types and functions.
-- The word "polymorphism" comes from Greek and means "having many forms": something which is polymorphic works for multiple types.

-- Polymorphic data types 
data List t = E | C t (List t) -- We can't reuse Cons and Empty since we already used those for the constructors of IntList.

-- t is type variavle which can stand for any type. (type variable must start with lower case.)

-- data List t means that the List type is parameterized by a type, in much the same way that a function can be parameterized by some input.

-- Examples:
-- You must give a type to support t.

lst1 :: List Int -- a List t consists of either the constructor E, or the constructor C along with a value of type t and another.
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'X' (C 'Y' (C 'z' E))

-- Polymorphic functions
filterList _ E  = E
filterList p (C x xs)
    | p x       = C x (filterList p xs)
    | otherwise = filterList p xs

-- The type is : (t -> Bool) -> List t -> List t
-- This can be read as "for any type t, filterIntList takes one function which from t -> Bool, and a list of t's, and return a lsit of t's"

-- One important thing to remember about polymorphcic functions is that the caller gets to pick the types.
-- When write a polymorphic function, it must work for every possible input.

-- The Prelude
-- is a module with a bunch of standard definitions that gets implicitly imported into every Haskell program.

-- Total and partial functions
-- Functions which have certain inputs that will crash the program or make them recurse infinitely.
-- Total funtions : are well-defined on all possible inputs.

-- avoiding partial function is a good style in an programming language.

-- head is a mistake. (don't use head, tail, init, last and !!)

-- We can replace the partial function by using pattern-matching.

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))
-- It can avoid the empty situation.

doStuff2 :: [Int] -> Int
doStuff2 []  = 0
doStuff2 [_] = 0
doStuff2 (x1:x2:_) = x1 + x2

-- They did the same thing, but the second one is better, because it was more obvious and easy to read.

-- Writing partial function.
-- There are two approaches to take.
-- 1. change the output type of the function to indicate the possible failure.
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

-- The advantage of the safe-mode:
-- 1. safeHead will never crash.
-- 2. The type of safeHead makes it obvious that it may fail for some inputs.
-- 3. The type system ensures that users of safeHead must appropriately check the return value of 
--    safeHead to see whether they got a value or Nothing.	

-- The goal is to have the types tell us as much as possible about the behavior of functions.
-- If we can guarante that some situations never happen.
-- if some condition is really guaranteed, the the types ought to reflect the guarantee.

data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x : xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []       = Nothing
listToNel (x : xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
