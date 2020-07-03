module Golf where

takenThElem :: [a] -> Int -> [a]
-- The first thing we must do is take apart this question into
-- simper one, we must figure out how to find nth element in the list.
-- I zip two list to indicate the position of the element in the lst.
-- if condition satisifies, get the first element of the x.
takenThElem lst n = [ fst x | x <- zip lst [1..length lst], (snd x) `rem` n == 0]

skips :: [a] -> [[a]]
-- As described in the specification, the length of the result must be same as
-- the length of the origin list.
skips lst = zipWith takenThElem (repeat lst) [1..length lst]

-- Exercise 2 Local maxima
--needs pattern matching
localMaxima :: [Integer] -> [Integer]
localMaxima (lft:bse:rht:remain)
    | bse > lft && bse > rht = bse : localMaxima (bse:rht:remain)
    | otherwise              = localMaxima (bse:rht:remain)
localMaxima _                = []

-- Exercise 3 Histogram
histogram :: [Integer] -> String
histogram xs = unlines (transToStar counted height) ++ "==========\n0123456789\n"
    where counted = count xs
          height  = maximum counted

-- Calculate the height of list [0..9] and trans them into [Int].
count :: [Integer] -> [Int]
count xs = [ length (filter (== x) xs) | x <- [0..9] ]

-- xs is the Int list to the times of each number in [Int 0 .. 9].
-- If the number greater than the current number, then print *
transToStar :: [Int] -> Int -> [String]
transToStar _ 0        = [] 
transToStar lst height = [[ if i >= height then '*' else ' ' | i <- lst ]] ++ transToStar lst (height - 1) 
