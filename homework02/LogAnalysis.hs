{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage message = case words message of
    ("I" : timestamp : xs)          -> LogMessage Info (read timestamp) (unwords xs)
    ("W" : timestamp : xs)          -> LogMessage Warning (read timestamp) (unwords xs)
    ("E" : degree : timestamp : xs) -> LogMessage (Error $ read degree) (read timestamp) (unwords xs)
    msg                             -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse messages  = map parseMessage (lines messages)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) originTree      = originTree
insert m@(LogMessage _ _ _) Leaf = Node Leaf m Leaf
insert o@(LogMessage _ timestamp _) (Node leftH (LogMessage _ otimestamp _) rightH)
    | timestamp >= otimestamp      = Node leftH o (insert o rightH)
    | otherwise                    = Node (insert o leftH) o rightH
insert _ tree                      = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree
build (x:xs) = insert x (build xs)
build []     = Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf             = []
inOrder (Node leftH mes rightH) = (inOrder leftH) ++ [mes] ++ inOrder rightH

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logmes  = case inOrder $ build logmes of
    ((LogMessage (Error level) _ mes) : remain)
        | level >= 50 -> mes : whatWentWrong remain
        | otherwise   -> []  : whatWentWrong remain
    ((LogMessage _ _ _) : remain) -> whatWentWrong remain
    ((Unknown _) : remain)        -> whatWentWrong remain
    []                -> []

