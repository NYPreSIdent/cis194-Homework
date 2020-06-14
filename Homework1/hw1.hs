-- Validating Credit Card Numbers!

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0     = []
    | otherwise  = n `rem` 10 : (toDigitsRev (n `div` 10))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []           = []
doubleEveryOther (x : [])     = [x]
doubleEveryOther (x : y : []) = [2 * x, y]
doubleEveryOther lst = doubleEveryOther (take ((length lst) - 2) lst) ++ doubleEveryOther (drop ((length lst) - 2) lst)

--Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits n
    | head n > 10 = (div (head n) 10) + (rem (head n) 10) + sumDigits (tail n)
    | otherwise   = (head n) + sumDigits (tail n)

--Exercise 4
validate :: Integer -> Bool
validate n 
    | (sumDigits (doubleEveryOther (toDigits n))) `rem` 10 == 0 = True
    | otherwise                                                 = False

--Exercise 5 The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoiOfOne :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiOfOne n src goal temp
    | n <= 0      = []
    | n == 1      = [(src, goal)]
    | otherwise   = hanoiOfOne (n - 1) src temp goal ++ hanoiOfOne 1 src goal temp ++ hanoiOfOne (n - 1) temp goal src

hanoiOfTow :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiOfTow n start middle end
    | n <= 0      = []
    | n == 1      = [(start, end)]
    | otherwise   = hanoiOfTow (n - 1) start end middle ++ hanoiOfTow 1 start middle end ++ hanoiOfTow (n - 1) middle start end

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour n start
