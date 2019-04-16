module Proc.Types where

import Data.Stack

data Program = Program Expression

newtype Identifier = Identifier String deriving (Eq, Show)

instance (Eq a) => Eq (Stack a) where
    (==) x y = case (stackPop x, stackPop y) of
                 (Nothing, Nothing) -> True
                 (Just (s1, e1), Just (s2, e2)) -> e1 == e2 && s1 == s2
                 otherwise -> False


data Expression = Number Int
                | Minus Expression Expression
                | Negate Expression
                | Mult Expression Expression
                | Div Expression Expression
                | IsZero Expression
                | IfThenElse Expression Expression Expression
                | Var String
                | LetIn Identifier Expression Expression
                | Proc Identifier Expression (Stack (String, Expression))
                | Call Expression Expression
                deriving (Eq, Show)
