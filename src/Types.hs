module Types where

data Program = Program Expression

data Expression = Number Int
                | Minus Expression Expression
                | Negate Expression
                | Mult Expression Expression
                | Div Expression Expression
                | IsZero Expression
                | IfThenElse Expression Expression Expression
                | Identifier String
                | LetIn Expression Expression Expression
                deriving (Eq, Show)
