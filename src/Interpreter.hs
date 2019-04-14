module Interpreter where

import Data.Stack

import Types

type Environment = Stack (String, Expression)

emptyEnv :: Environment
emptyEnv = stackNew

run :: Program -> Expression
run (Program e) = evaluate emptyEnv e

evaluate :: Environment -> Expression -> Expression
evaluate _   (Number n) = Number n
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
evaluate env (IsZero e) = if (evaluate env e) == Number 0 then Number 1 else Number 0
evaluate env (IfThenElse (Number 1) t _) = evaluate env t
evaluate env (IfThenElse (Number 0) _ f) = evaluate env f
evaluate env (IfThenElse e t f) = evaluate env (IfThenElse (evaluate env e) t f)
evaluate env (LetIn (Identifier i) v b) = evaluate (stackPush env (i, v)) b
evaluate env (Identifier s) =
    case stackPop env of
      Just (newEnv, (i, v)) ->
          if i == s then
              evaluate newEnv v
          else evaluate newEnv (Identifier s)
      Nothing -> Identifier s
