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
                expr = translate (LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Number 77) (Number 11)) emptyEnv) (Call (Var "f") (Number 0)))
                res  = evaluate expr
            res `shouldBe` Const 66
        it "Call stored function with substitution" $ do
            let
                expr = translate $ LetIn (Identifier "f") (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv) (Call (Var "f") (Number 77))
                res  = evaluate expr
            res `shouldBe` Const 66
        it "Simple Currying" $ do
            -- for some reason Currying in my DeBruijn implementation takes
            -- the arguments in reverse order
            let
                expr = translate (Call
                                    (Call
                                        (Proc (Identifier "x")
                                            (Proc (Identifier "y") (Minus (Var "x") (Var "y")) emptyEnv)
                                        emptyEnv) (Number 5)
                                    ) (Number 3)
                               )
                res = evaluate expr
            res `shouldBe` Const (negate 2)
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
--       it "75b" $ do
--          let
--              expr = translate $ Call (Proc (Identifier "f") (Call (Var "f") (Call (Var "f") (Number 77)) )emptyEnv) (Proc (Identifier "x") (Minus (Var "x") (Number 11)) emptyEnv)
--              res  = evaluate expr
--          res `shouldBe` Const 55
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
            res `shouldBe` Const (negate 1)
    describe "exercises" $ do
        it "3.20, currying" $ do
            let
                expr = translate $ LetIn
                        (Identifier "f")
                        (Proc (Identifier "x") (Proc (Identifier "y") (Minus (Var "x") (Var "y")) emptyEnv) emptyEnv)
                        (Call (Call (Var "f") (Number 5)) (Number 3))
                res = evaluate expr
            res `shouldBe` Const (negate 2)
--      it "3.25, factorial" $ do
--          let
--              expr = translate $ LetIn
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
--                      (Call (Call (Var "f") (Number 1)) (Number 5))
--              res = evaluate expr
--          res `shouldBe` Const 120

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
