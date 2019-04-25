module UnitDeBruijn where

import Test.Hspec

import Proc.Interpreter (emptyEnv)
import Proc.Types
import DeBruijn.Interpreter
import DeBruijn.Types
import DeBruijn.Translator

evaluate_test :: Spec
evaluate_test = do
    describe "simple" $ do
        it "Call function" $ do
            let
                expr = translate $ Call (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv) (Number 77)
                res  = evaluate expr
            res `shouldBe` Const 66
        it "Call stored function" $ do
            let
                expr = (LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Number 77) (Number 11)) emptyEnv) (Call (Var "f") (Number 0)))
                tr = translate expr
                res  = evaluate tr
            print tr
            res `shouldBe` Const 66
--      it "Call stored function with substitution" $ do
--          let
--              env  = pushEnv emptyEnv (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv)
--              expr = Call (Var "f") (Number 77)
--              res  = evaluate env expr
--          res `shouldBe` Number 66
--      it "Simple Currying" $ do
--          let
--              env = emptyEnv
--              expr = (Call
--                          (Call
--                              (Proc (Identifier "x")
--                                  (Proc (Identifier "y") (Minus (Var "x") (Var "y")) emptyEnv)
--                              emptyEnv) (Number 5)
--                          ) (Number 3)
--                     )
--              res = evaluate env expr
--          res `shouldBe` Number 2
--  describe "example expressions" $ do
--      it "75a" $ do
--          let
--              env  = emptyEnv
--              expr = LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv) (Call (Var "f") (Call (Var "f") (Number 77)))
--              res  = evaluate env expr
--          res `shouldBe` Number 55
--      it "75b" $ do
--          let
--              env  = emptyEnv
--              expr = Call (Proc (Identifier "f") (Call (Var "f") (Call (Var "f") (Number 77)) )emptyEnv) (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv)
--              res  = evaluate env expr
--          res `shouldBe` Number 55
--      it "76" $ do
--          let
--              env = emptyEnv
--              expr = LetIn (Identifier "x") (Number 200) (
--                       LetIn (Identifier "f") (Proc (Identifier "z") (Minus (Var "z") (Var "x")) emptyEnv) (
--                          LetIn (Identifier "x") (Number 100) (
--                              LetIn (Identifier "g") (Proc (Identifier "z") (Minus (Var "z") (Var "x")) emptyEnv)
--                                  (Minus (Call (Var "f") (Number 1)) (Call (Var "g") (Number 1)))
--                              )
--                          )
--                      )
--              res = evaluate env expr
--          res `shouldBe` Number (negate 100)
--  describe "exercises" $ do
--      it "3.20, currying" $ do
--          let
--              env = emptyEnv
--              expr = LetIn
--                      (Identifier "f")
--                      (Proc (Identifier "x") (Proc (Identifier "y") (Minus (Var "x") (Var "y")) emptyEnv) emptyEnv)
--                      (Call (Call (Var "f") (Number 5)) (Number 3))
--              res = evaluate env expr
--          res `shouldBe` Number 2
--      it "3.25, factorial" $ do
--          let
--              env = emptyEnv
--              expr = LetIn
--                      (Identifier "f")
--                      (Proc (Identifier "x")
--                          (Proc (Identifier "y")
--                              (IfThenElse (IsZero (Var "x"))
--                                  (Var "y")
--                                  (Call (Call (Var "f") (Minus (Var "x") (Number 1))) (Mult (Var "x") (Var "y"))
--                                  )
--                              )
--                          emptyEnv)
--                      emptyEnv)
--                      (Call (Call (Var "f") (Number 5)) (Number 1))
--              res = evaluate env expr
--          res `shouldBe` Number 120

translate_test :: Spec
translate_test = do
    describe "example expressions" $ do
        it "93" $ do
            let
                expr = LetIn (Identifier "x") (Number 37)
                         (Proc (Identifier "y")
                             (
                                LetIn (Identifier "z") (Minus (Var "y") (Var "x"))
                                      (Minus (Var "z") (Var "y"))
                             )
                             emptyEnv
                         )
                res = NLetIn (Const 37)
                             (NProc (NLetIn (NMinus (NVar 1) (NVar 0)) (NMinus (NVar 2) (NVar 1))))
            translate expr `shouldBe` res
