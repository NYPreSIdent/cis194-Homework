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
insert m Leaf                      = Node Leaf m Leaf
insert (Unknown _) originTree      = originTree
insert o@(LogMessage _ timestamp _) (Node leftH ori@(LogMessage _ otimestamp _) rightH)
    | timestamp >= otimestamp      = Node leftH ori (insert o rightH)
    | otherwise                    = Node (insert o leftH) ori rightH
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
-- Solution depends on @jumblesale from his GitHub.

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong mess = transToString $ inOrder $ build (filter (isCondition 50) mess) 

isCondition :: Int -> LogMessage -> Bool
isCondition minLevel (LogMessage (Error level) _ _)
    | level > minLevel = True
    | otherwise        = False
isCondition _ _        = False

transToString :: [LogMessage] -> [String]
transToString (LogMessage _ _ msg : msgs) = msg : transToString msgs
transToString _                           = []
