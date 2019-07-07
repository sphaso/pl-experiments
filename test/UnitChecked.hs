module UnitChecked where

import Test.Hspec
import Data.Stack

import Checked.Types
import Checked.Checker

evaluateTest :: Spec
evaluateTest = do
    describe "simple" $ do
        it "Minus" $ do
            let expr = Minus (Number 34) (Number 33)
            NumVal `shouldBe` typeOf expr
        it "LetIn Minus" $ do
            let expr = LetIn NumVal (Identifier "x") NumVal (Number 34) (Minus (Var "x") (Number 33))
            NumVal `shouldBe` typeOf expr
        it "IfThenElse" $ do
            let expr = LetIn NumVal (Identifier "x") BoolVal (George True) (IfThenElse (Var "x") (Number 5) (Number 6))
            NumVal `shouldBe` typeOf expr
        it "Call Proc" $ do
            let expr = Call (Proc (Identifier "x") NumVal (Minus (Var "x") (Number 2))) (Number 3)
            NumVal `shouldBe` typeOf expr
        it "Proc Type" $ do
            let expr = LetIn NumVal (Identifier "x") NumVal (Number 34) (Proc (Identifier "x") NumVal (Minus (Var "x") (Number 2)))
            FunkVal (NumVal, NumVal) `shouldBe` typeOf expr
    describe "example expressions" $ do
        it "75a" $ do
            let expr = LetIn NumVal (Identifier "f") (FunkVal (NumVal, NumVal)) (Proc (Identifier "x") NumVal (Minus (Var "x") (Number 11))) (Call (Var "f") (Call (Var "f") (Number 77)))
            NumVal `shouldBe` typeOf expr
        it "75b" $ do
            let expr = Call (Proc (Identifier "f") (FunkVal (NumVal, NumVal)) (Call (Var "f") (Call (Var "f") (Number 77)))) (Proc (Identifier "x") NumVal (Minus (Var "x") (Number 11)))
            NumVal `shouldBe` typeOf expr
        it "76" $ do
            let
                expr = LetIn NumVal (Identifier "x") NumVal (Number 200) (
                         LetIn NumVal (Identifier "f") (FunkVal (NumVal, NumVal)) (Proc (Identifier "z") NumVal (Minus (Var "z") (Var "x"))) (
                            LetIn NumVal (Identifier "x") NumVal (Number 100) (
                                LetIn NumVal (Identifier "g") (FunkVal (NumVal, NumVal)) (Proc (Identifier "z") NumVal (Minus (Var "z") (Var "x")))
                                    (Minus (Call (Var "f") (Number 1)) (Call (Var "g") (Number 1)))
                                )
                            )
                        )
            NumVal `shouldBe` typeOf expr
    describe "exercises" $ do
        it "3.20, currying" $ do
            let
                expr = LetIn
                        NumVal
                        (Identifier "f")
                        (FunkVal (NumVal, FunkVal (NumVal, NumVal)))
                        (Proc (Identifier "x") NumVal (Proc (Identifier "y") NumVal (Minus (Var "x") (Var "y"))))
                        (Call (Call (Var "f") (Number 5)) (Number 3))
            NumVal `shouldBe` typeOf expr
--      it "3.25, factorial" $ do
--          let
--              expr = LetIn
--                      NumVal
--                      (Identifier "f")
--                      (FunkVal (NumVal, FunkVal (NumVal, NumVal)))
--                      (Proc (Identifier "x") NumVal
--                          (Proc (Identifier "y") NumVal
--                              (IfThenElse (IsZero (Var "x"))
--                                  (Var "y")
--                                  (Call (Call (Var "f") (Minus (Var "x") (Number 1))) (Mult (Var "x") (Var "y"))
--                                  )
--                              )
--                          )
--                      )
--                      (Call (Call (Var "f") (Number 5)) (Number 1))
--          NumVal `shouldBe` typeOf expr
