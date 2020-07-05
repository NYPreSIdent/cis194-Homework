-- Exercise1 : Wholemeal programming.
func1' :: [Integer] -> Integer
func1' = product . map (subtract 2) . filter even

func2' :: Integer -> Integer
func2' = sum . filter even . takeWhile (> 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise2 : Folding the tree.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertBanlanced Leaf 

insertBanlanced :: a -> Tree a -> Tree a
insertBanlanced elem Leaf              = Node 0 Leaf elem Leaf
insertBanlanced elem t@(Node height left origin right)
    | getWeight left > getWeight right = if isPerfectTree t 
                                         then Node (height + 1) left origin $ insertBanlanced elem right 
                                         else Node height left origin $ insertBanlanced elem right
    | otherwise                        = if isPerfectTree t
                                         then Node (height + 1) (insertBanlanced elem left) origin right 
                                         else Node height (insertBanlanced elem left) origin right

isPerfectTree :: Tree a -> Bool
isPerfectTree Leaf = True;
isPerfectTree t@(Node height _ _ _)
    | getWeight t ==  2 ^ (height + 1) - 1 = True
    | otherwise                            = False

getWeight :: Tree a -> Integer
getWeight Leaf                  = 0
getWeight (Node _ left _ right) = getWeight left + getWeight right + 1

xor :: [Bool] -> Bool
xor = foldl (\acc x -> if acc == x then False else True) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Exercise (optional)
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base $ reverse xs 

-- Exercise 4: Finding primes:
cartPtod :: [a] -> [b] -> [(a, b)]
cartPtod xs ys = [(x, y) | x <- xs, y <- ys]

seive :: Integer -> [Integer]
seive n = map (\x -> 2 * x + 1) . filter (\x -> notElem x nums) $ [1..n] where nums = removedNum [1..n]

removedNum :: [Integer] -> [Integer]
removedNum xs = map (\(x, y) -> x + y + 2*x*y) . filter (\t -> fst t <= snd t) $ cartPtod xs xs
