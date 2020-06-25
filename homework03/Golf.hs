module Golf where

skips :: [a] -> [[a]]
skips (x:xs) =   
skips [x]    = [x]
skips []     = []

skipHelper :: [a] -> Int -> [[a]]
skipHelper 
