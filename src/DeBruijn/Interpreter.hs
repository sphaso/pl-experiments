module DeBruijn.Interpreter where

import Data.Stack

import DeBruijn.Types

type Environment = Stack NamelessExpression

extendEnv :: Environment -> NamelessExpression -> Environment
extendEnv = stackPush

evaluate :: NamelessExpression -> NamelessExpression
evaluate = evaluate' stackNew

evaluate' :: Environment -> NamelessExpression -> NamelessExpression
evaluate' _   c@(Const _) = c
evaluate' _   (NMinus (Const a) (Const b)) = Const (a - b)
evaluate' env (NMinus i j) = evaluate' env (NMinus (evaluate' env i) (evaluate' env j))
evaluate' _   (NMult (Const a) (Const b)) = Const (a * b)
evaluate' env (NMult i j) = evaluate' env (NMult (evaluate' env i) (evaluate' env j))
evaluate' _   (NPlus (Const a) (Const b)) = Const (a + b)
evaluate' env (NPlus i j) = evaluate' env (NPlus (evaluate' env i) (evaluate' env j))
evaluate' env (NIsZero b) = if evaluate' env b == Const 1 then Const 1 else Const 0
evaluate' env (NIfThenElse c t f) = if evaluate' env c == Const 1 then evaluate' env t else evaluate' env f
evaluate' env (NLetIn e b) = evaluate' newE b
    where
        newE = extendEnv env (evaluate' env e)
evaluate' env (NProc b) = NClosure b env
evaluate' _   (NClosure b e) = NClosure b e
evaluate' env (NCall v@(NVar _) e) = evaluate' env (NCall (evaluate' newE v) e)
    where
        newE = extendEnv env e
evaluate' env (NCall (NProc b) a) = evaluate' (extendEnv env arg) b
    where
        arg = evaluate' env a
evaluate' env (NCall (NClosure b e) a) = evaluate' (extendEnv e arg) b
    where
        arg = evaluate' env a
evaluate' env (NCall b e) = evaluate' newE (NCall (evaluate' newE b) e)
    where
        newE = extendEnv env e
evaluate' env (NVar 0) = case stackPop env of
                           Nothing -> error "out of bounds"
                           Just (_, e)  -> e
evaluate' env (NVar n) = case stackPop env of
                           Nothing -> error "oops"
                           Just (e, _) -> evaluate' e (NVar (n-1))
