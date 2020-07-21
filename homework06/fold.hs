import Prelude hiding (foldr, foldl)

veryBigList = [1..1000000]

-- Start with the following:
foldr f z []     = z
foldr f z (x:xs) = x `f` foldr f z xs

sum1 = foldr (+) 0

try1 = sum1 veryBigList
-- This will cause stack overflow
-- The problem is that the (+) operator is strict in both of its arguments.
-- This means that both arguments must be fully evaluated before (+) can return a result.
-- This is the process:
-- 1 + (2 + (3 + 4 + (...)))
-- 1 is pushed on the stack. Then:
-- 2 + (3 + (4 + (...))) is evaluated.
-- and 2 is pushed into stack....
-- This will cause a big chain of (+) and cause stack overflow.

-- One problem with the chain of (+)'s is that it can't be made reduced until the very last moment,
-- when it's already too late.
-- The reason we can't reduce it is that the chain doesn't contain an expression which can be reduced (a redex, for reducible expression)
-- if instead of the chian 1 + (2 + 3 + (4 + (...))) we could form the chain (((0 + 1) + 2) + 3) ..., then we would be a redex.

foldl f z []     = z
foldl f z (x:xs) = let z' = z `f` x
                   in foldl f z' xs -- tail recursion.

sum2 = foldl (+) 0

try2 = sum2 veryBigList
-- But it still cause stack overflow.
-- Well, you clearly see that the redexes are created. But instead of being directly reduced, they allocated on the heap.

-- Seq is a primitive function that when applied to x and y will first reduce x then return y.
-- The idea is that y references x so that when y is reduced x will not be a big unreduced chain anymore.

foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs 
