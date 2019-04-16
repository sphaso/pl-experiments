module UnitProc where

import Test.Hspec
import Data.Stack

import Proc.Types
import Proc.Interpreter

evaluate_test :: Spec
evaluate_test = do
    describe "simple" $ do
        it "Call function" $ do
            let
                env  = emptyEnv
                expr = Call (Proc (Identifier "x") (Minus (Var "x") (Number 11)) env) (Number 77)
                res  = evaluate env expr
            res `shouldBe` Number 66
        it "Call stored function" $ do
            let
                env  = pushEnv emptyEnv (Identifier "f") (Proc (Identifier "x") (Minus (Number 77) (Number 11)) emptyEnv)
                expr = Call (Var "f") (Number 0)
                res  = evaluate env expr
            res `shouldBe` Number 66
        it "Call stored function with substitution" $ do
            let
                env  = pushEnv emptyEnv (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv)
                expr = Call (Var "f") (Number 77)
                res  = evaluate env expr
            res `shouldBe` Number 66
    describe "example expressions" $ do
        it "75a" $ do
            let
                env  = emptyEnv
                expr = LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv) (Call (Var "f") (Call (Var "f") (Number 77)))
--              expr = LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv) (Call (Var "f") (Number 77))
                res  = evaluate env expr
            res `shouldBe` Number 55
--      it "75b" $ do
--          let
--              env  = emptyEnv
--              expr = Call (Proc (Identifier "f") (Call (Var "f") (Call (Var "f") (Number 77)))) (Proc (Identifier "x") (Minus (Var "x") (Number 11)))
--              res  = evaluate env expr
--          res `shouldBe` Number 55
--      it "76" $ do
--          let
--              env = emptyEnv
--              expr = LetIn (Identifier "x") (Number 200) (
--                       LetIn (Identifier "f") (Proc (Identifier "z") (Minus (Var "z") (Var "x"))) (
--                          LetIn (Identifier "x") (Number 100) (
--                              LetIn (Identifier "g") (Proc (Identifier "z") (Minus (Var "z") (Var "x")))
--                                  (Minus (Call (Var "f") (Number 1)) (Call (Var "g") (Number 1)))
--                              )
--                          )
--                      )
--              res = evaluate env expr
--          res `shouldBe` Number 100
