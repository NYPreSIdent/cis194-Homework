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

class Expr where
