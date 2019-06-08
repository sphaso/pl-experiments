module UnitCPS where

import Test.Hspec
import Data.Stack

import CPS.Types
import CPS.Interpreter

evaluateTest :: Spec
evaluateTest = do
    describe "simple" $ do
        it "Minus" $ do
            let expr = Minus (Number 34) (Number 33)
                res = run $ Program expr
            Number 1 `shouldBe` res
        it "LetIn Minus" $ do
            let expr = LetIn (Identifier "x") (Number 34) (Minus (Var "x") (Number 33))
                res = run $ Program expr
            Number 1 `shouldBe` res
        it "IfThenElse" $ do
            let expr = LetIn (Identifier "x") (Number 1) (IfThenElse (Var "x") (Number 5) (Number 6))
                res = run $ Program expr
            Number 5 `shouldBe` res
        it "Call Proc" $ do
            let expr = Call (Proc (Identifier "x") (Minus (Var "x") (Number 2))) (Number 3)
                res = run $ Program expr
            Number 1 `shouldBe` res
    describe "example expressions" $ do
        it "75a" $ do
            let
                expr = LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11))) (Call (Var "f") (Call (Var "f") (Number 77)))
                res  = run $ Program expr
            res `shouldBe` Number 55
        it "75b" $ do
            let
                expr = Call (Proc (Identifier "f") (Call (Var "f") (Call (Var "f") (Number 77)))) (Proc (Identifier "x") (Minus (Var "x") (Number 11)))
                res  = run $ Program expr
            res `shouldBe` Number 55
        it "76" $ do
            let
                expr = LetIn (Identifier "x") (Number 200) (
                         LetIn (Identifier "f") (Proc (Identifier "z") (Minus (Var "z") (Var "x"))) (
                            LetIn (Identifier "x") (Number 100) (
                                LetIn (Identifier "g") (Proc (Identifier "z") (Minus (Var "z") (Var "x")))
                                    (Minus (Call (Var "f") (Number 1)) (Call (Var "g") (Number 1)))
                                )
                            )
                        )
                res = run $ Program expr
            res `shouldBe` Number (negate 100)
