module DeBruijn.Types where

data NamelessExpression = Const Int
                        | NMinus NamelessExpression NamelessExpression
                        | NNegate NamelessExpression
                        | NMult NamelessExpression NamelessExpression
                        | NDiv NamelessExpression NamelessExpression
                        | NIsZero NamelessExpression
                        | NIfThenElse NamelessExpression NamelessExpression NamelessExpression
                        | NVar Int
                        | NLetIn NamelessExpression NamelessExpression
                        | NProc NamelessExpression
                        | NCall NamelessExpression NamelessExpression
                deriving (Eq, Show)
