module ExplicitRefs.Types where

import Data.Stack

newtype Program = Program Expression

newtype Identifier = Identifier String deriving (Eq, Show)

instance (Eq a) => Eq (Stack a) where
    (==) x y = case (stackPop x, stackPop y) of
                 (Nothing, Nothing) -> True
                 (Just (s1, e1), Just (s2, e2)) -> e1 == e2 && s1 == s2
                 _ -> False

data Expression = Number Int
                | Minus Expression Expression
                | IsZero Expression
                | IfThenElse Expression Expression Expression
                | Var String
                | LetIn Identifier Expression Expression
                | Closure Identifier Expression (Stack (String, Expression))
                | Proc Identifier Expression
                | Call Expression Expression
                | SetRef Identifier Expression
                | DeRef Identifier
                | NewRef Expression
                | Ref Int
                | IO [Expression]
                deriving (Eq, Show)
