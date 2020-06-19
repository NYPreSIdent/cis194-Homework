removeNonUpperCase st = [ c | c <- st, elem c ['A' .. 'Z']]

-- Read
read "5" :: Int -- type annotations
read "5" :: Float
(read "5" :: Float) * 4
read "[1,2,3,4]" :: [Int]
read "(3, 'a')" :: (Int, Char)

-- Enum
["a" .. "e"]
[LT .. GT]
[3 .. 5]
succ 'B'
pred 'B'

-- Bounded
minBound :: Int
maxBound :: Char
maxBound :: Bool
minBound :: Bool