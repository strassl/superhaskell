import Test.HUnit
import Test.QuickCheck

{-
import Lib

double_withZero_shouldReturnZero :: Test
double_withZero_shouldReturnZero = TestCase $ assertEqual "0*x != 0" 0 (double 0)

prop_absAlwaysGreaterOrEqual :: Int -> Bool
prop_absAlwaysGreaterOrEqual x = (abs (x * 2)) >= (abs x)

prop_zeroAbsorbingElement :: Int -> Bool
prop_zeroAbsorbingElement x = (x * 0) == 0

main :: IO ()
main = do
    runTestTT $ TestList [double_withZero_shouldReturnZero]
    quickCheck prop_absAlwaysGreaterOrEqual
    quickCheck prop_zeroAbsorbingElement
-}

main = putStrLn "Not implemented"
