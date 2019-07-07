module Checked.Types where

newtype Identifier = Identifier String deriving (Eq, Show)

data Type = NumVal | BoolVal | FunkVal (Type, Type) deriving (Eq, Show)

data Expression = Number Int
                | George Bool
                | IsZero Expression
                | Minus Expression Expression
                | Mult Expression Expression
                | LetIn Type Identifier Type Expression Expression
                | IfThenElse Expression Expression Expression
                | Proc Identifier Type Expression
                | Call Expression Expression
                | Var String
