module UnitDeBruijn where

import Test.Hspec

import Proc.Interpreter (emptyEnv)
import Proc.Types
import DeBruijn.Interpreter
import DeBruijn.Types
import DeBruijn.Translator

evaluateTest :: Spec
evaluateTest = do
    describe "simple" $ do
        it "Call function" $ do
            let
                expr = translate $ Call (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv) (Number 77)
                res  = evaluate expr
            res `shouldBe` Const 66
        it "Call stored function" $ do
            let
                expr = translate (LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Number 77) (Number 11)) emptyEnv) (Call (Var "f") (Number 0)))
                res  = evaluate expr
            res `shouldBe` Const 66
        it "Call stored function with substitution" $ do
            let
                expr = translate $ LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv) (Call (Var "f") (Number 77))
                res  = evaluate expr
            res `shouldBe` Const 66
        it "Simple Currying" $ do
            let
                expr = translate (Call
                                    (Call
                                        (Proc (Identifier "x")
                                            (Proc (Identifier "y") (Minus (Var "x") (Var "y")) emptyEnv)
                                        emptyEnv) (Number 5)
                                    ) (Number 3)
                               )
                res = evaluate expr
            res `shouldBe` Const 2
    describe "example expressions" $ do
         it "page 66b" $ do
           let
               expr = translate $ LetIn (Identifier "z") (Number 5)
                                     (LetIn (Identifier "x") (Number 3)
                                         (LetIn (Identifier "y") (Minus (Var "x") (Number 1))
                                             (LetIn (Identifier "x") (Number 4) (Minus (Var "z")
                                                (Minus (Var "x") (Var "y"))))
                                         )
                                     )
               res = evaluate expr
           res `shouldBe` Const 3
         it "page 67" $ do
           let
               expr = translate $ LetIn (Identifier "x") (Number 7)
                                    (LetIn (Identifier "y") (Number 2)
                                        (LetIn (Identifier "y")
                                            (LetIn (Identifier "x") (Minus (Var "x") (Number 1))
                                                (Minus (Var "x") (Var "y")))
                                            (Minus (Minus (Var "x") (Number 8)) (Var "y"))
                                        )
                                    )
               res = evaluate expr
           res `shouldBe` Const (negate 5)
         it "75a" $ do
            let
                expr = translate $ LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv) (Call (Var "f") (Call (Var "f") (Number 77)))
                res  = evaluate expr
            res `shouldBe` Const 55
         it "75b" $ do
            let
                expr = translate $ Call (Proc (Identifier "f") (Call (Var "f") (Call (Var "f") (Number 77)) )emptyEnv) (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv)
                res  = evaluate expr
            res `shouldBe` Const 55
         it "76" $ do
            let
                expr = LetIn (Identifier "x") (Number 200) (
                         LetIn (Identifier "f") (Proc (Identifier "z") (Minus (Var "z") (Var "x")) emptyEnv) (
                            LetIn (Identifier "x") (Number 100) (
                                LetIn (Identifier "g") (Proc (Identifier "z") (Minus (Var "z") (Var "x")) emptyEnv)
                                    (Minus (Call (Var "f") (Number 1)) (Call (Var "g") (Number 1)))
                                )
                            )
                        )
                res = evaluate $ translate expr
            res `shouldBe` Const (negate 100)
         it "89" $ do
            let
                expr = translate $ LetIn (Identifier "x") (Number 3) (
                          LetIn (Identifier "y") (Number 4)
                             (Plus
                                 (LetIn (Identifier "x")
                                     (Plus (Var "y") (Number 5))
                                     (Mult (Var "x") (Var "y")))
                                  (Var "x")
                             )
                        )
                res = evaluate expr
            res `shouldBe` Const 39
    describe "exercises" $
        it "3.20, currying" $ do
            let
                expr = translate $ LetIn
                        (Identifier "f")
                        (Proc (Identifier "x") (Proc (Identifier "y") (Minus (Var "x") (Var "y")) emptyEnv) emptyEnv)
                        (Call (Call (Var "f") (Number 5)) (Number 3))
                res = evaluate expr
            res `shouldBe` Const 2

translateTest :: Spec
translateTest =
    describe "example expressions" $ do
        it "92" $ do
            let
               expr = LetIn (Identifier "x") (Number 42)
                         (
                             LetIn (Identifier "y") (Number 93)
                                (Minus (Var "x") (Var "y"))
                         )
               res = NLetIn (Const 42)
                             (NLetIn (Const 93)
                                     (NMinus (NVar 1) (NVar 0))
                             )
            translate expr `shouldBe` res
        it "93" $ do
            let
                expr = LetIn (Identifier "x") (Number 37)
                         (Proc (Identifier "y")
                             (
                                LetIn (Identifier "z") (Minus (Var "y") (Var "x"))
                                      (Minus (Var "x") (Var "y"))
                             )
                             emptyEnv
                         )
                res = NLetIn (Const 37)
                             (NProc (NLetIn (NMinus (NVar 0) (NVar 1)) (NMinus (NVar 2) (NVar 1))))
            translate expr `shouldBe` res
        it "3.20, currying" $ do
            let
                expr = translate $ LetIn
                        (Identifier "f")
                        (Proc (Identifier "x") (Proc (Identifier "y") (Minus (Var "x") (Var "y")) emptyEnv) emptyEnv)
                        (Call (Call (Var "f") (Number 5)) (Number 3))
            expr `shouldBe` NLetIn (NProc (NProc (NMinus (NVar 1) (NVar 0)))) (NCall (NCall (NVar 2) (Const 5)) (Const 3))

