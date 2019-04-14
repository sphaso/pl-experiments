import Test.Hspec
import Test.QuickCheck

import qualified UnitLet

main :: IO ()
main = hspec UnitLet.evaluate_test
