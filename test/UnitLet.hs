module UnitLet where

import Test.Hspec
import Data.Stack

import Let.Types
import Let.Interpreter

evaluate_test :: Spec
evaluate_test = do
    describe "simple" $ do
        it "simple operation" $ do
            let
               res = evaluate emptyEnv (Minus (Number 3) (Number 1))
            res `shouldBe` Number 2
        it "simple substitution" $ do
            let
                env = stackPush emptyEnv ("x", Number 5)
                expr = Minus (Identifier "x") (Number 2)
                res = evaluate env expr
            res `shouldBe` Number 3
        it "IsZero" $ do
            let env = stackPush emptyEnv ("x", Number 33)
                expr = IsZero (Minus (Identifier "x") (Number 11))
                res = evaluate env expr
            res `shouldBe` Number 0
        it "IfThenElse" $ do
            let env = emptyEnv
                expr = IfThenElse (Number 0) (Number 5) (Number 10)
                res = evaluate env expr
            res `shouldBe` Number 10
        it "Mult" $ do
            let env = stackPush emptyEnv ("x", Number 3)
                expr = Mult (Identifier "x") (Number 3)
                res = evaluate env expr
            res `shouldBe` Number 9

    describe "example expressions" $ do
        it "page 64" $ do
            let env = stackPush (stackPush (stackPush emptyEnv ("i", Number 1)) ("v", Number 5)) ("x", Number 10)
                expr = Minus (Minus (Identifier "x") (Number 3)) (Minus (Identifier "v") (Identifier "i"))
                res = evaluate env expr

            res `shouldBe` Number 3
        it "page 66a" $ do
            let env = stackPush (stackPush emptyEnv ("x", Number 33)) ("y", Number 22)
                expr = IfThenElse (IsZero (Minus (Identifier "x") (Number 11))) (Minus (Identifier "y") (Number 2)) (Minus (Identifier "y") (Number 4))
                res = evaluate env expr
            res `shouldBe` Number 18
        it "page 66b" $ do
            let env = emptyEnv
                expr = LetIn (Identifier "z") (Number 5)
                        (LetIn (Identifier "x") (Number 3)
                            (LetIn (Identifier "y") (Minus (Identifier "x") (Number 1))
                                (LetIn (Identifier "x") (Number 4) (Minus (Identifier "z") (Minus (Identifier "x") (Identifier "y"))))
                            )
                        )
                res = evaluate env expr
            res `shouldBe` Number 3
        it "page 67" $ do
            let env = emptyEnv
                expr = LetIn (Identifier "x") (Number 7)
                        (LetIn (Identifier "y") (Number 2)
                            (LetIn (Identifier "y")
                                (LetIn (Identifier "x") (Minus (Identifier "x") (Number 1)) (Minus (Identifier "x") (Identifier "y")))
                                (Minus (Minus (Identifier "x") (Number 8)) (Identifier "y"))
                            )
                        )
                res = evaluate env expr
            res `shouldBe` Number (negate 5)

