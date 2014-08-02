module MuCalc.Utils where

import Data.Either (either)
import qualified Data.Set as S
import MuCalc.MuFormula hiding (iff, implies)
import MuCalc.States
import MuCalc.Realization
import MuCalc.MuModel
import Test.QuickCheck
import Test.HUnit hiding (Test, State)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Framework

isLeft = either (const True) (const False)
isRight = not . isLeft

implies p q = (not p) || q
iff p q = (p `implies` q) && (q `implies` p)

--Chain these guys for maximum fun
extract :: State s => Realization s -> ([s] -> Property) -> Property
extract (Left e) = error $ show e --ignore the given property computation
extract (Right states) = ($states) --apply the given computation to the set

assertRealization :: State s => Realization s -> ([s] -> Assertion) -> Assertion
assertRealization (Left e) = error $ show e
assertRealization (Right states) = ($states)

setNthElement :: [a] -> Int -> a -> [a]
setNthElement xs i val = fnt ++ val : bck
  where fnt = fst pair
        bck = tail (snd pair)
        pair = splitAt i xs

zipProperties :: [(String, Property)] -> [Test]
zipProperties = uncurry (zipWith testProperty) . unzip

zipTestCases :: [(String, Assertion)] -> [Test]
zipTestCases = uncurry (zipWith testCase) . unzip
