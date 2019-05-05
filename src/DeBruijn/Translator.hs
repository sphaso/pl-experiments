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
translate' env (Plus a b) = NPlus (translate' env a) (translate' env b)
translate' env (Mult a b) = NMult (translate' env a) (translate' env b)
translate' env (IsZero a) = NIsZero (translate' env a)
translate' env (IfThenElse c t f) = NIfThenElse (translate' env c) (translate' env t) (translate' env f)
translate' env (LetIn (Identifier i) e b) = NLetIn (translate' env e) (translate' newE b)
    where
        newE = extendEnv env i
translate' env (Proc (Identifier i) b _) = NProc (translate' newE b)
    where
        newE = extendEnv env i
translate' env (Call p expr) = NCall (translate' env p) (translate' env expr)
translate' env (Var x) = case elemIndices x env of
                           []  -> error ("cannot find " ++ x)
                           els -> NVar (minimum els)
