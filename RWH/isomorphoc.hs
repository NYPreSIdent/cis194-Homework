-- isomorphic
data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- Binary Tree Type
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show) 

toList (Cons x xs) = x : toList xs
toList Nil         = []

-- error message
-- :type error: String -> a
mySecond :: [a] -> a

mySecond xs = if null (tail xs)
              then error "list too short"
	          else head (tail xs)

tidySecond :: [a] -> Maybe a

tidySecond (_:x:_) = Just x
tidySecond _       = Nothing 
-- The first pattern only matches if the list
-- is at least two elements long(it contanis two list constructors),
-- and it binds the variable x to the list's second element. The second pattern
-- is matched if the first fails.

-- local function and global variables.
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"
