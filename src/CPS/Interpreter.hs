module CPS.Interpreter where

import Data.Stack

import CPS.Types

type Environment = Stack (String, Expression)

emptyEnv :: Environment
emptyEnv = stackNew

pushEnv :: Environment -> Identifier -> Expression -> Environment
pushEnv env (Identifier i) exp = stackPush env (i, exp)

run :: Program -> Expression
run (Program e) = evaluate emptyEnv e IdCont

applyCont :: Continuation -> Expression -> Expression
applyCont (LetCont i b env c) (Proc v f) = evaluate (pushEnv env i (Closure v f e1)) b c
    where
        e1 = pushEnv env i (Closure v f e1)
applyCont (LetCont i b env c) expr = evaluate (pushEnv env i expr) b c
applyCont (Diff1Cont e env c) e1 = evaluate env e (Diff2Cont e1 c)
applyCont (Diff2Cont (Number a) c) (Number b) = applyCont c (Number (a - b))
applyCont (Mult1Cont e env c) e1 = evaluate env e (Mult2Cont e1 c)
applyCont (Mult2Cont (Number a) c) (Number b) = applyCont c (Number (a * b))
applyCont (IfCont _ f env c) (Number 0) = evaluate env f c
applyCont (IfCont t _ env c) (Number 1) = evaluate env t c
applyCont (CallCont arg env c) f@Closure{} = evaluate env arg (ArgCont f c)
applyCont (CallCont arg env c) (Proc i x) = evaluate env arg (ArgCont (Closure i x env) c)
applyCont (ArgCont (Closure i b e) c) arg = evaluate newE b c
    where
        newE = pushEnv e i arg
applyCont IdCont x = x

evaluate :: Environment -> Expression -> Continuation -> Expression
evaluate _   n@(Number _) c = applyCont c n
evaluate env (Proc i x) c = applyCont c (Proc i x)
evaluate env c@Closure{} cont = applyCont cont c
evaluate env (Minus e1 e2) c = evaluate env e1 (Diff1Cont e2 env c)
evaluate env (Mult e1 e2) c = evaluate env e1 (Mult1Cont e2 env c)
evaluate env (IsZero (Number 0)) c = applyCont c (Number 1)
evaluate env (IsZero _) c = applyCont c (Number 0)
evaluate env (IfThenElse e t f) c = evaluate env e (IfCont t f env c)
evaluate env (LetIn i v b) c = evaluate env v (LetCont i b env c)
evaluate env (Call f arg) c = evaluate env f (CallCont arg env c)
evaluate env (Var s) c = applyCont c (fromEnv env s)

fromEnv env s =
    case stackPop env of
      Just (newEnv, (i, v)) ->
          if i == s then v
          else fromEnv newEnv s
      Nothing -> error ("cannot find " ++ s)
