import Test.Hspec
import Test.QuickCheck

import qualified UnitLet
import qualified UnitProc
import qualified UnitDeBruijn

main :: IO ()
main = hspec (
                 UnitLet.evaluate_test
               >> UnitProc.evaluate_test
               >> UnitDeBruijn.translate_test
               >> UnitDeBruijn.evaluate_test
             )
