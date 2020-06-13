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
    | n < 0                                                     = False
    | (sumDigits (doubleEveryOther (toDigits n))) `rem` 10 == 0 = True
    | otherwise                                                 = False  