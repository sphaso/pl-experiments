{-# LANGUAGE OverloadedStrings #-}

module UnitExplicitRefs where

import Test.Hspec
import Data.Stack

import ExplicitRefs.Types
import ExplicitRefs.Interpreter

evaluateTest :: Spec
evaluateTest = do
    describe "simple" $ do
        it "new, deref" $ do
            let
                expr = Program $ LetIn (Identifier "x") (NewRef (Number 6)) (DeRef (Identifier "x"))
                res = run expr
            res `shouldBe` Number 6
        it "new, set, deref" $ do
            let
                expr = LetIn (Identifier "x") (NewRef (Number 0)) (IO [SetRef (Identifier "x") (Number 2), DeRef (Identifier "x")])
                res = run $ Program expr
            res `shouldBe` Number 2

    describe "examples" $ do
        it "even only" $ do
            -- Watch out! It should result in an infinite loop if you test
            -- for an odd number, which is a bit stupid I'll admit
            let
                even = LetIn (Identifier "x") (NewRef (Number 6))
                        (LetIn (Identifier "even")
                             (Proc (Identifier "dummy")
                                   (IfThenElse (IsZero (DeRef (Identifier "x")))
                                           (Number 1)
                                           (IO [SetRef (Identifier "x") (Minus (DeRef (Identifier "x")) (Number 2)), Call (Var "even") (Number 777)])
                                   )
                             )
                             (Call (Var "even") (Number 777)))
                res = run $ Program even
            res `shouldBe` Number 1
        it "private var" $ do
            let
                expr = LetIn (Identifier "g")
                             (LetIn (Identifier "counter") (NewRef (Number 0))
                                    (Proc (Identifier "dummy")
                                          (IO [SetRef (Identifier "counter") (Minus (DeRef (Identifier "counter")) (Number (negate 1))), DeRef (Identifier "counter")])))
                             (LetIn (Identifier "a") (Call (Var "g") (Number 11))
                                    (LetIn (Identifier "b") (Call (Var "g") (Number 11))
                                           (Minus (Var "a") (Var "b"))
                                    )
                             )
                res = run $ Program expr
            res `shouldBe` Number (negate 1)
