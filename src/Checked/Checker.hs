module Checked.Checker where

import Checked.Types
import Data.Stack

type Environment = Stack (String, Type)

emptyEnv :: Environment
emptyEnv = stackNew

pushEnv :: Environment -> Identifier -> Type -> Environment
pushEnv env (Identifier i) typ = stackPush env (i, typ)

typeOf :: Expression -> Type
typeOf expr = typeOf' expr emptyEnv

typeOf' :: Expression -> Environment -> Type
typeOf' (Number _) _ = NumVal
typeOf' (George _) _ = BoolVal
typeOf' (IsZero x) env = if xType == NumVal then BoolVal else error "type error!"
  where
    xType = typeOf' x env
typeOf' (Mult x y) env = typeOf' (Minus x y) env -- let me tell you about the Thunk life
typeOf' (Minus x y) env = if xType == NumVal && yType == NumVal then NumVal else error $ "type error! " ++ show xType ++ " vs. " ++ show yType
  where
    xType = typeOf' x env
    yType = typeOf' y env
--  typeOf' (LetIn tres l tl (Proc i t b) bl) env | tres == bType = blType
--                                                | otherwise = error "type error!"
--    where
--      bType = typeOf' b (pushEnv (pushEnv env l t) i (FunkVal (t, tres)))
--      blType = typeOf' bl (pushEnv env i (FunkVal (t, tres)))
typeOf' (LetIn _ l tl w b) env | wType == tl = typeOf' b (pushEnv env l wType)
                               | otherwise = error $ "type error! " ++ show wType ++ " vs. " ++ show tl
  where
    wType = typeOf' w env
typeOf' (IfThenElse b t f) env | typeOf' b env /= BoolVal = error "type error!"
                              | otherwise = let
                                              tType = typeOf' t env
                                              fType = typeOf' f env
                                            in
                                              if tType /= fType then
                                                error "type error!"
                                              else
                                                tType

typeOf' (Call f a) env | argType /= paramType = error $ "type error! " ++ show argType ++ " vs. " ++ show paramType
                      | otherwise = retType
  where
    FunkVal (paramType, retType) = typeOf' f env
    argType = typeOf' a env
typeOf' (Proc i t b) env = FunkVal (t, typeOf' b newEnv)
  where
    newEnv = pushEnv env i t
typeOf' (Var s) env =
    case stackPop env of
      Just (newEnv, (i, v)) ->
          if i == s then v
          else typeOf' (Var s) newEnv
      Nothing -> error ("cannot find " ++ s)

