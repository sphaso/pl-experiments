module DeBruijn.Translator (translate) where

import Data.List (elemIndices)

import Proc.Types
import DeBruijn.Types

extendEnv :: [String] -> String -> [String]
extendEnv l v = v:l

translate :: Expression -> NamelessExpression
translate e = translate' [] e

translate' :: [String] -> Expression -> NamelessExpression
translate' _ (Number n) = Const n
translate' env (Minus a b) = NMinus (translate' env a) (translate' env b)
translate' env (LetIn (Identifier i) e b) = NLetIn (translate' newE e) (translate' newE b)
    where
        newE = extendEnv env i
translate' env (Proc (Identifier i) b _) = NProc (translate' newE b)
    where
        newE = extendEnv env i
translate' env (Call (Var i) expr) = NLetIn (translate' env (Var i)) (translate' env expr)
translate' env (Call (Proc i b _) expr) = translate' env (LetIn i expr b)
translate' env (Var x) = case elemIndices x (reverse env) of
                           []  -> error ("cannot find " ++ x)
                           els -> NVar (maximum els)
