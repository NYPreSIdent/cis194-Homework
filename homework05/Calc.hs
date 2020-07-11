module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit a)     = a
eval (Add a1 a2) = eval a1 + eval a2
eval (Mul a1 a2) = eval a1 * eval a2

evalStr :: String -> Maybe Integer
evalStr expr = case parseExp Lit Add Mul expr of
    Just a  -> Just (eval a)
    Nothing -> Nothing

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    add x y = Add x y
    mul x y = Mul x y
    lit x   = Lit x

reify :: ExprT -> ExprT
reify = id
