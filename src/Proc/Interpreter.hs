module Proc.Interpreter where

import Data.Stack

import Proc.Types

type Environment = Stack (String, Expression)

emptyEnv :: Environment
emptyEnv = stackNew

pushEnv :: Environment -> Identifier -> Expression -> Environment
pushEnv env (Identifier i) exp = stackPush env (i, exp)

run :: Program -> Expression
run (Program e) = evaluate emptyEnv e

evaluate :: Environment -> Expression -> Expression
evaluate _   n@(Number _) = n
evaluate env (Proc i x _) = Proc i x env
evaluate _   (Minus (Number a) (Number b)) = Number (a - b)
evaluate env (Minus e1 e2) = evaluate env (Minus a b)
    where
        a = evaluate env e1
        b = evaluate env e2
evaluate env (Negate e) = Minus (Number 0) (evaluate env e)
evaluate _   (Mult (Number a) (Number b)) = Number (a * b)
evaluate env (Mult e1 e2) = evaluate env (Mult a b)
    where
        a = evaluate env e1
        b = evaluate env e2
evaluate _   (Div (Number a) (Number b)) = Number (div a b)
evaluate env (Div e1 e2) = evaluate env (Div a b)
    where
        a = evaluate env e1
        b = evaluate env e2
evaluate env (IsZero e) = if evaluate env e == Number 0 then Number 1 else Number 0
evaluate env (IfThenElse (Number 1) t _) = evaluate env t
evaluate env (IfThenElse (Number 0) _ f) = evaluate env f
evaluate env (IfThenElse e t f) = evaluate env (IfThenElse (evaluate env e) t f)
evaluate env (LetIn i (Proc v f e) b) = evaluate (pushEnv env i (Proc v f e1)) b
   where
-- this genius idea was put forward by my colleague Francesco who's
-- obviously a bloody recursive wizard. Thank you
       e1 = pushEnv env i (Proc v f e1)
evaluate env (LetIn i v b) = evaluate (pushEnv env i v) b
evaluate env (Call (Var s) e) = evaluate env (Call (evaluate env (Var s)) e)
evaluate env (Call (Proc i e procE) x) = evaluate (pushEnv procE i arg) e
    where
        arg = evaluate env x
evaluate env (Call f arg) = evaluate env (Call (evaluate env f) arg)
evaluate env (Var s) =
    case stackPop env of
      Just (newEnv, (i, v)) ->
          if i == s then v
          else evaluate newEnv (Var s)
      Nothing -> error ("cannot find " ++ s)
