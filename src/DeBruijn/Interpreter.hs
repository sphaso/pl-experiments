module DeBruijn.Interpreter where

import qualified Data.Map.Strict as M

import DeBruijn.Types

type Environment = M.Map Int NamelessExpression

extendEnv :: Environment -> NamelessExpression -> Environment
extendEnv env expr = M.insert i expr env
    where
        i = case M.lookupMax env of
              Nothing     -> 0
              Just (k, _) -> k + 1

evaluate :: NamelessExpression -> NamelessExpression
evaluate = evaluate' M.empty

evaluate' :: Environment -> NamelessExpression -> NamelessExpression
evaluate' _   c@(Const _) = c
evaluate' _   (NMinus (Const a) (Const b)) = Const (a - b)
evaluate' env (NMinus i j) = evaluate' env (NMinus (evaluate' env i) (evaluate' env j))
evaluate' env (NLetIn p@(NProc _) e) = evaluate' (extendEnv env p) e
evaluate' env (NLetIn e b) = evaluate' newE b
    where
        fakeE = extendEnv env e
        newE = extendEnv env (evaluate' fakeE e)
evaluate' env (NProc b) = evaluate' env b
evaluate' env (NCall b e) = evaluate' (extendEnv env arg) b
    where
        arg = evaluate' env e
evaluate' env (NVar i) = case env M.!? i of
                          Nothing -> error ("index " ++ (show i) ++ " out of bounds")
                          Just expr -> evaluate' env expr
