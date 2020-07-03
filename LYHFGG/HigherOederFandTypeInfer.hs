-- Haskell functions can take functions as parameters and return functions as return values.
-- This is called higher order function.
-- if you want to define a computation by defining what stuff is intead of defining steps that change
-- some states and maybe looping them, higher order funtion is indispensible.

-- Curried funtions
-- Every Function in Haskell officially only takes one parameter -- depends on the lambda calculus.
-- Every funtion that accepted several parameters so far have been curried funtions.
-- Sample:
max 4 5
-- which is same as:
(max 4) 5 -- max funtion takes two parameter, and return the bigger one. This process first creates a funtion
-- that takes a parameter and returns either 4 or that parameter, depending on which is bigger.Then, 5 is applied to the
-- function and that function produces our desired result.

-- Putting a space between two things is simply function application.
-- The space is sort of like an operator and it has the highest precedence.
max :: (Ord a) => a -> a -> a -- can be written as
max :: (Ord a) => a -> (a -> a)
-- can be read as: max takes an a and returns a function that takes an a and returns an a.
-- That's why the return type and the parameters of funtions are all simply separated with arrows.

-- partially applied function: calling function with too few parameters.
-- This is a neat way to create functions on the fly so we can pass them to another or to seed them with some data.
-- Partially applied function is meaning that a function that takes as many parameters as we left out.
multThree :: (Num) => a -> a -> a -> a
multThree x y z = x * y * z

multThree 3 5 9 is same as ((multThree 3) 5) 9
-- First, 3 is applied to multThree, because they're separated by a space.
-- That creates a function that will take a parameter and then multiply it by 15.
-- The type of this function can be written as:
multThree :: (Num) => a -> (a -> (a -> a)) -- The thing before the -> is the parameter that a function takes and the thing after it
-- is what it returns. So, our funtion takes an a and returns a funtion : (Num a) a -> (a -> a).
-- we can write it as this:
let multiTwoWithNine = multThree 9
let multiWithEighteen = multiTwoWithNine 2

-- Create a funtion that takes a number and compares it to 100:
comparesWithHundred :: (Num a, Ord a) => a -> Ordering
comparesWithHundred x = compare 100 x -- This funtion is partially applied.

-- We can rewrite that funtion like this:
comparesWithHundred' :: (Num a, Ord a) => a -> Ordering
comparesWithHundred' = compare 100
-- compare :: (Ord a) => a -> (a -> Ordering) is same as the type of the compareWithHundred'

-- Infix functions can also be partially applied by using sections.
-- To section an infix function, smply surround it with parentheses and only supply a parameter
-- on one side.

-- GHCi doesn't kown how to print a function. Functions aren't  instances of the Show typeclass, so we can't
-- get a neat string representation of a function. For example, if we do 1 + 1 at the GHCi prompt, it first calulates
-- that number and then calls Show on 2 to get string 2 -- the representation of number 2.

-- Some higher-orderism is in order.
-- Functions can take funtions as parameters and also return functions.
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- Notice that before we didn't need parentheses because -> is naturally
-- right-associative.However, here, they're mandatory.
-- They indicate that the first parameter is function taht takes something and the retunr value is 
-- also of the same type. We can just say that this funtion takes two parameters and returns one thing
-- The fisrt parameter is a function (of type a -> a) and the second is that same a.

-- The awesomeness and usefulness of partial application is evident.If our function requires us to pass it a
-- function that takse only one parameter, we can just partially apply a function to the point where it takes
-- one parameter and then pass it.

-- We can use this feature to implement zipWith which is a function in standard library.
-- It takes a function and two lists as parameters and then joins the two lists by applying the function between
-- corresponding elements.

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' [] _ _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- A single higher order function can be used for a multitude of diiferent tasks if it's general enough.
-- The diffrence between functional programming and imperative programming:
-- a single higher order function can be used in very versatile ways.
-- Imperative programming usually uses stuff like for loops, while loops, setting something to a variable, checking its state, etc;
-- to achieve some behavior and then wrap it around interface, like a function.
-- Functional programming uses higher order functions to abstract away common patterns, like examing two lists in pairs and doing something
-- with those pairs or getting a set of solutions and eliminating the ones you don't need.

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
-- We can think ahead and write what their end result would be if they were called fully applied.

-- Some examples using map:
map (+3) [1,5,3,1,6]
map (++ "!") ["BIFF", "BANG", "POW"]
map (replicate 3) [3..6]
-- All the things can be rewrriten as list comprehension, but the readable will be decresed.
-- especially once you're dealing with maps of maps and the the whole thing with a lot of brackets
-- can get a bit messy.

-- and filter can also be replaced by list comprehension, you just have to decide what's more readable depending on the code
-- and the context. 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted  = quicksort (filter (>x)  xs)
    in smallerSorted ++ [x] ++ biggerSorted
-- Mapping and filtering is the bread and butter of every functional programmer's toolbox.
-- In functional programming, we can solve problem by listing all possible answers and filter it.
-- for exaplems, in imperative programming, you need three loops to find the right triangle that satisfy some conditions
-- , in Haskell, we just map that function over a list of values and then we filter the resulting list out for the results that satisfy
-- out search.
-- Thanks to Haskell's laziness, even we map someting over a list several times and filter it several times, it will only
-- pass over the list once.
-- 1. Find the largest number under 100,000 that's divisible by 3829.
-- To do that, we'll just filter a set of possibilities in which we kow the solution lies.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0
-- if the condition satisfied, the proceed will end, no matter how large our list it is.

-- takeWhile: takes a predicate and a list and then goes from the beggining of the list and returns its elements while the predicate holes
-- true

-- find the sum of all odd sqares that are smaller than 10,000
sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- We could also write this using list comprehensions.
sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

-- Collatz sequences
-- If a number is even, we divide it by two.If it's odd, we multiply if by 3
-- and then add 1 to that.
-- This sequence always produce a bunch of numbers always fininished at 1.
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (3 * n + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- Applying one parameter to a function that takes two parameters returns a function that takes one parameter.
-- function (*) has a type of (Num) a => a -> a -> a.
-- If map (*) over a list [0..], we get back a list of fuctions that only take one parameter
-- so (Num a) => [a -> a], like this [(2*), (3*)]

let listOfFuns = map (*) [0..]
(listOfFuns !! 4) 5
-- Getting the element with the index 4 from our list returns a function that's equivalent to (4*).

-- Lambdas
-- Lambdas are basically anonymous funtions that are used because we need some functions only once.
-- Normally, we make a lambda with the sole purpose of passing it to a higher-order function.
-- To make a lambda, we write a \ (lol, because it kind of looks like greek letter lambda if you squint hard enough)
-- and write parameters and separated by spaces.
-- After that, we use -> to indicate lambda function body. We usually surround them by parentheses, because otherwise they
-- extend all the way to the right.
-- We can replace isLong by using lambda calculus in our numLongChains.
-- Such that:
numLongChains' :: Int
numLongChains' = length (filter (\ xs -> length xs > 15) (map chain [1..100]))

-- Cautions: People who are not well acquainted with how currying and partial application works often use lambdas where they
-- don't need to.
-- in this case: map (+3) [1,6,3,2] === map (\x -> x + 3) [1,6,3,4]
-- using lambda function is stupid, since using partial application is much more readable.

-- Like normal function, you can pattern match in lambdas.
-- The only difference is that you can't define several patterns for one parameter, like making (x:xs) and [] for one parameter.
-- Notice these two functions are equivalent:
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

-- We write filp like this is very cool:
flip' :: (a -> b -> c) -> b -> a -> c
filp' f = \x y -> f y x
-- This is more explicit that your function is mainly meant to be partially applied and passed on to a function as a parameter.

-- Only folds and horses
-- When we do recurse on a list, we must care about edge case.
-- Because the (x:xs) pattern is so common that we create some useful function to encapsulate some operations.
-- These functions are called folds.

-- A fold takes a binary function and a starting value, and a list to fold up.
-- The binary function is called with the accumulator and the first elememnt and produces a new accumulator.
-- flodl -- left fold: It folds the list up from the left side.
sum' :: (Num a) => [a] -> a
sum' xs = flodl (\acc x -> acc + x) 0 xs

-- And we can rewrite like this:
sum' :: (Num) => [a] -> a
sum' = foldl (+) 0 -- this will return a function that takes a list.
-- Generally, if you have a function like foo a = bar b a, you can write it as foo = bar b, because of currying.
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = flodl (\acc x -> if x == y then True else acc) False ys

-- right fold binary function has the accumulator as the second parameter, and current element as the first parameter.
-- It kind of makes sense that the right fold has the accumulator on the right, because it folds from the right side.
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- we can also use left fold to implement map'
-- like this: map' f xs = foldl (/acc x -> acc ++ [f x]) [] xs
-- but we must notice that the ++ is much more expensive than :
-- so we usually use foldr when we're building up new lists from a list.
-- foldr can flod over a infinite list, and foldl can not.

-- Tips: Folds can be used to implement any function where you traverse a list once, element by element, and then produce something
-- based on that. Whenever you want to traverse a list to return something, chances are you want a fold.

-- a bunch of powerful utilies of folds.
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc) -- foldr1 works on lists which can not work on empty.

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' func xs = foldr (\acc x -> if func x then x : acc else acc)	[]

head' :: [a] -> a
head' = foldl1 (\x _ -> x)

last' :: [a] -> a
last' = foldr1 (\_ x -> x) -- head is better implemented by pattern matching.
-- We can rewrite it looks like this: flodl (filp (:)) [] -- It's awesome.

-- We can use (f 3 (f 4 (f 5 (f 6 z)))) to represent foldr f z [3,4,5,6] sounds like a kind of inner function.
-- left fold like this: (f (f ( f (f 3 z) 4) 5) 6)
-- If we use flip (:) as the binary function:
-- flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6)
-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumlator states in the form of a list.

-- Scans are used to monitor the progression of a function that can be implemented as a fold.
sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl (+) (map sqrt [1..]))) + 1 -- We use takeWhile here instead of filter because filter doesn't work on infinite lists.
-- So we use takeWhile to cut the scanlist off at the first occurence of a sum greater than 1000.

-- Function application with $
-- Function application with a space is left-associative (so f a b c is the same as ((f a) b) c), function application with $ is right-associative and it has lowest precedence of any oprator.
-- When a $ is encountered, the expression on its right is applied as the parameter to the function on its left. 
-- Because $ is right-associative, f (g (z x)) is equal to f $ g $ z x.
-- sum (filter (> 10) (map (*) [2..10])) can be rewrite as sum & filter (> 10) $ map (*) [2..10]
map ($ 3) [(4+), (10*), (^2), sqrt]
-- $ means that function application can be treated just like another function.

-- function composision is meaning that composing two functions produces a new function that, when called with a parameter, say, x is the equivalent of calling g with the parameter x and then
-- calling the f with that result.
-- like this: (.) :: (b -> c) -> (a -> b) -> a -> c
-- f must take as its parameter a value that has the same type as g's return value.
-- One of the uses for function composition is making functions on the fly to pass to other functions.

-- We can use function composision to instead of lambda expression:
map (\x -> negate (abs x)) [1,2,3,4,5]

map (negate . abs) [1,2,3,4,5]
-- They are ablotulely same.
-- We can compose many functions at a time.
-- The expression f (g (z x)) is equuivalent to (f . g . z) x
map (\xs -> negate (sum (tail xs))) [1,2,3,4]
map (negate . sum . tail) [[1..5], [3..6], [1..7]]

-- We can use partially application to compose multiple parameters function.

sum (replicate 5 (max 6.7 8.9)) 
sum . replicate 5 . max 6.7 $ 8.9
-- a function that takes what max 6.7 takes and applies replicate 5 to it is created.
-- Then, a function that takes the result of that does a sum if it is created.
-- Finally, that function is called with 8.9.
-- If you want to replace parentheses by using dot, the number of parentheses is as 2 times as dot.

-- Point free style (also called the pointless style)
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs
--  Because of Currying, we can omit the xs on the both side, because calling foldl (+) 0
-- creates a function that takes a list of a and return a value of a.
-- so the sum' = foldl (+) 0 is called point free style.

-- another point free style example:
fn x = ceiling (negate (tan (cos (max 50 x))))

-- In order to get rid of x on both sides, we can rewrite this function by composision.
-- only (max 50) wonldn't make sense. We can't get the cosine of a function.
-- We can rewrite it like this:
fn = ceiling . negate . tan . cos . max 50
-- The point free style can urge you think about functions and what kind of functions composing them
-- results instead of thinking about data and how it's shuffled around.
-- The prefered style is to use let bindings to give labels to intermediary result or split the problem into
-- sub-problems and then put it together so that the function makes sense to someone reading is instread of 
-- just making a huge composision chain.

-- There are two examples to show the difference between them.
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum :: Integer
oddSquareSum =
	let oddSquares = filter odd $ map (^2) [1..]
	    belowLimit = takeWhile (<10000) oddSquares
	in  sum belowLimit
-- The second one is more readable but won't win any code golf competition.
