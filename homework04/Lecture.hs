-- anonymous function (lambda abstraction)
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (\x -> x > 100) xs
-- Defining a anonymous function means that we won't need to define a normal function and binds a name to it
-- and pass the name.
-- (The \ backslash is supposed to look kind of like a lambda with the leg missing)
-- And this is a better way to write greaterThan100:
greaterThan100' xs = filter (>100) xs
-- (>100) is a operator section: (?y) is equivalent to the function \x -> x ? y
-- and (y?) is equivalent to this: \x -> y ? x.
-- Using an operator section allows us to partially apply an operator to one of its two arguments.

-- Function composision
-- Write a function of type (b -> c) -> (a -> b) -> (a -> c)
-- foo f g = ...
-- In the place of the ... we need to write a function of type a -> c.
foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f = \x -> f (g x) -- We have a function g which can turn an a into a b, and a function can turn b to c.
-- foo is really called (.), and represents function representation.
-- That is, if f and g are functions, then f . g is the function which does first g and then f.
-- Function composision can be quite useful in writing concise, elegent code. It fits well in a "wholemeal" style
-- where we think about composing together successive high-level transformations of a data structure.
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100 -- myTest' is just a pipeline composed of three smaller functions.

-- It looks weired when encounter the type of the multi-argument functions, like they have "extra" arrows in them.
f :: Int -> Int -> Int
f x y = 2 * x + y

-- There is a beautiful and deep reason for this: All functions in Haskell take only one argument!
-- Function arrows associate to the right:
-- W -> X -> Y -> Z is equuivalent to W -> (X -> (Y -> Z))
-- We can always add or remove parenthses around the right most top-level arrow in a type.
-- Function application is left-associative.
-- f 3 2 is really shorthand for (f 3) 2. -- This makes sense given what we said previously about f actually taking one argument and returning a function.
-- We apply f to an argument 3, which returns a function of type Int -> Int.

-- So the abbreviate (f 3) 2 can rewrite like this: f 3 2 which stands as a "multi-argument" function.

-- function definition and lambda abstraction are just syntax suger for
-- \x -> (\y -> (\z -> ...)) and f = \x -> (\y -> (\z -> ...))

-- If we actually represent a function of two arguments we can use a single argument which is a tuple.
f'' :: (Int, Int) -> Int
f'' (x, y) = 2 * x + y -- which can also be thought of as thinking "two arguments", but it just is one pair (one argument).

-- The standard library provides two functions to convert the two represetations of a two-argument function.
schonfinkel :: ((a, b) -> c) -> a -> b -> c
schonfinkel f x y = f (x, y)

unschonfinkel :: (a -> b -> c) -> (a, b) -> c
unschonfinkel f (x, y) = f x y

-- like this:
uncurry (+) 2 3 -- This is very useful when you have a pair and want to apply a function to it.

-- Partial application
-- because the curry feature of Haskell, the partial application particularly easy.
-- The idea of partial application function is that we can take a function of multiple arguments and apply it to just some of its arguments.
-- Every function is Haskell can be "partially applied" to its first argument, resulting in a function of the remaining arguments.
-- and operator function may be an exception.

-- And there is an art to deciding the order of arguments to a function to make partial applications of it as useful as possible: the arguments should be ordered
-- from from "least to greatest variation", that is, arguments which will often be the same should be listed first, and arguments which will often be diffrent should come last.

-- Wholemeal programming
foobar :: [Integer] -> Integer
foobar []       = 0
foobar (x:xs)
    | x > 3     = (7*x + 2) + foobar xs
    | otherwise = foobar xs -- The problem is that:
-- 1. Doing too much at once, 2. Working at too low of a level.

-- Instead of thinking about what we want to do with each element, we can instead think about making incremental transformations to the entire list,
-- using the existing patterns that we know of.
-- The difference between two thinking: one is thinking about the single element, the specific element, and the other is thinking about the entire list.
foobar' :: [Integer] -> Integer
foobar' = sum . map . (\x -> 7*x + 2) . filter (>3)
-- foobar' is defined as a pipeline of three functions: fisrt, we throw away all elements from the list which are not greater than three;
-- next, we apply an arithmatic operation to every element of the remaining list; finally, we sum the results.

-- Point free style: This style of coding in which we define a function without reference to its argument --- in some sense saying
-- what a function is rather than what it does. (It'a pipeline of three functions rather than add 1 add 2 until ...)

-- Folds:
-- A few functions on lists that follow a similar function: all of them somehow "combine" the elements of the elements of the list into a final answer.
-- There are lots of functions:
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- can be abstracted as this:
fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z
fold z f (x:xs) = f x (flod z f xs)
-- Notice how fold essentially replaces [] with z and (:) with f.
fold f z [a,b,c] == a `f` (b `f` (c `f` z))

-- sum, product, any, and, or all these things are defined using fold.
foldr f z [a, b, c] == a `f` (b `f` (c `f` z))
foldl f z [a, b, c] == (((z `f` a) `f` b) `f` c)
-- We shold use fold' from Data.List instead, which is more efficient. 