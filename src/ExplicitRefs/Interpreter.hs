module ExplicitRefs.Interpreter where

import Data.Stack

import ExplicitRefs.Types

type Environment = Stack (String, Expression)
type Store = Stack (String, Expression)

emptyEnv :: Environment
emptyEnv = stackNew

emptyStore :: Store
emptyStore = stackNew

pushEnv :: Environment -> Identifier -> Expression -> Environment
pushEnv env (Identifier i) exp = stackPush env (i, exp)

pushStore :: Store -> Identifier -> Expression -> Store
pushStore store (Identifier i) expr = stackPush store (i, expr)

run :: Program -> Expression
run (Program e) = v
    where (_, v) = evaluate emptyEnv e emptyStore

evaluate :: Environment -> Expression -> Store -> (Store, Expression)
evaluate _   n@(Number _) s = (s, n)
evaluate env (Proc i x) s = (s, Proc i x)
evaluate env c@Closure{} s = (s, c)
evaluate _   (Minus (Number a) (Number b)) s = (s, Number (a - b))
evaluate env (Minus e1 e2) s = evaluate env (Minus a b) s''
    where
        (s', a)  = evaluate env e1 s
        (s'', b) = evaluate env e2 s'
evaluate env (IsZero e) s = if z == Number 0 then (s', Number 1) else (s', Number 0)
    where
        (s', z) = evaluate env e s
evaluate env (IfThenElse (Number 1) t _) s = evaluate env t s
evaluate env (IfThenElse (Number 0) _ f) s = evaluate env f s
evaluate env (IfThenElse e t f) s = evaluate env (IfThenElse z t f) s'
    where
        (s', z) = evaluate env e s
evaluate env (LetIn i (LetIn j (NewRef expr) b) b') s = evaluate env (LetIn i b b') s'
    where
        (s', _) = evaluate env (SetRef j expr) s
evaluate env (LetIn i (NewRef expr) b) s = evaluate env b s'
    where
        (s', _) = evaluate env (SetRef i expr) s
evaluate env (LetIn i (Proc v f) b) s = evaluate (pushEnv env i (Closure v f e1)) b s
   where
       e1 = pushEnv env i (Closure v f e1)
evaluate env (LetIn i v b) s = evaluate (pushEnv env i v) b s
evaluate env (Call (Var s) e) store = evaluate env (Call x e) s'
    where
        (s', x) = evaluate env (Var s) store
evaluate env (Call (Proc i e) x) s = evaluate env (Call (Closure i e newE) arg) s'
    where
        (s', arg) = evaluate env x s
        newE = pushEnv env i arg
evaluate env (Call (Closure i ex c) x) s = evaluate (pushEnv c i x) ex s
evaluate env (Call f arg) s = evaluate env (Call x arg) s'
    where
        (s', x) = evaluate env f s
evaluate env (IO [e]) s = evaluate env e s
evaluate env (IO (x:xs)) s = evaluate env (IO xs) s'
    where
        (s', _) = evaluate env x s
evaluate env (SetRef j@(Identifier s) expr) store =
    case stackPop s' of
        Just (newStore, (i, v)) ->
            if i == s then (stackPush newStore (i, value), value)
            else evaluate env (SetRef j value) newStore
        Nothing -> (stackPush s' (s, value), value)
  where
    (s', value) = evaluate env expr store
evaluate env (DeRef j@(Identifier s)) store =
    case stackPop store of
      Just (newStore, (i, v)) ->
          if i == s then (store, v)
          else evaluate env (DeRef j) newStore
      Nothing -> error ("cannot find " ++ s)
evaluate env (Var s) store =
    case stackPop env of
      Just (newEnv, (i, v)) ->
          if i == s then (store, v)
          else evaluate newEnv (Var s) store
      Nothing -> error ("cannot find " ++ s)
