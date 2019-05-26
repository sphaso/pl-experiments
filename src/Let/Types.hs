module Let.Types where

newtype Program = Program Expression

newtype Identifier = Identifier String deriving (Eq, Show)

data Expression = Number Int
                | Minus Expression Expression
                | Negate Expression
                | Mult Expression Expression
                | Div Expression Expression
                | IsZero Expression
                | IfThenElse Expression Expression Expression
                | Var String
                | LetIn Identifier Expression Expression
                deriving (Eq, Show)
