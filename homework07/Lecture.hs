data Tree a = Empty
            | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty        = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty   = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- The patterns:
-- 1. takes a tree as input.
-- 2. pattern-matches on the input Tree
-- 3. in the Empty case, gives a simple answer.
-- 4. in the Node case:
--    1. calls itself recursively on both subtrees.
--    2. somehow combines the results from the recursive calls with the data x to produce the final result.

