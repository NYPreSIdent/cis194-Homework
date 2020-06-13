hailstone :: Integer -> Integer
hailstone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise         = 4
foo n
    | n < 0             = 0
    | n `mod` 17 == 2   = -43
    | otherwise         = n + 3

p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

nums, range, range2 :: [Integer]
nums = [1, 2, 3, 19]
range = [1..100]
range2 = [2, 4 .. 100]

-- String is just an abbreviation for [Char]

-- hailstone iteration:
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Functions on lists, compute the length of a list of Integers
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x : xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []             = [] -- Do nothing to the empty list.
sumEveryTwo (x : [])       = [x] -- Do nothing to lists with a single element
sumEveryTwo (x : (y : zs)) = (x + y) : sumEveryTwo zs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1 