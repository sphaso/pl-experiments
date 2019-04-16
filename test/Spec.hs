import Test.Hspec
import Test.QuickCheck

import qualified UnitLet
import qualified UnitProc

main :: IO ()
main = hspec (UnitLet.evaluate_test >> UnitProc.evaluate_test)
