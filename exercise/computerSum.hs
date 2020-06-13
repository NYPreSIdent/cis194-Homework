-- compute the sum of the integers from 1 to n

sumtorials :: Integer -> Integer
sumtorials 0 = 0
sumtorials n = n + sumtorials (n - 1)

y :: Int
y = y + 1