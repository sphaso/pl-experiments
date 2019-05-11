import Test.Hspec
import Test.QuickCheck

import qualified UnitLet
import qualified UnitProc
import qualified UnitDeBruijn

main :: IO ()
main = hspec (
                 UnitLet.evaluate_test
               >> UnitLet.parser_test
               >> UnitProc.evaluate_test
               >> UnitDeBruijn.translateTest
               >> UnitDeBruijn.evaluateTest
             )
