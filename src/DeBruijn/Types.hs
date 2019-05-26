module DeBruijn.Types where

import Data.Stack

instance (Eq a) => Eq (Stack a) where
    (==) x y = case (stackPop x, stackPop y) of
                 (Nothing, Nothing) -> True
                 (Just (s1, e1), Just (s2, e2)) -> e1 == e2 && s1 == s2
                 _ -> False

data NamelessExpression = Const Int
                        | NMinus NamelessExpression NamelessExpression
                        | NNegate NamelessExpression
                        | NPlus NamelessExpression NamelessExpression
                        | NMult NamelessExpression NamelessExpression
                        | NDiv NamelessExpression NamelessExpression
                        | NIsZero NamelessExpression
                        | NIfThenElse NamelessExpression NamelessExpression NamelessExpression
                        | NVar Int
                        | NLetIn NamelessExpression NamelessExpression
                        | NProc NamelessExpression
                        | NClosure NamelessExpression (Stack NamelessExpression)
                        | NCall NamelessExpression NamelessExpression
                deriving (Eq, Show)
