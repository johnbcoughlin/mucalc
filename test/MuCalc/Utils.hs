module MuCalc.Utils where

import Data.Either (either)
import qualified Data.Set as S
import MuCalc.MuFormula
import MuCalc.States
import MuCalc.Realization
import MuCalc.MuModel
import Test.QuickCheck
import Test.HUnit hiding (State, Test)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Framework

isLeft = either (const True) (const False)
isRight = not . isLeft

implies p q = (not p) || q
iff p q = (p `implies` q) && (q `implies` p)

--Chain these guys for maximum fun
extract :: Realization -> (StateSet -> Property) -> Property
extract (Left e) = error $ show e --ignore the given property computation
extract (Right set) = ($set) --apply the given computation to the set

assertRealization :: Realization -> (S.Set State -> Assertion) -> Assertion
assertRealization (Left error) = const (assert False)
assertRealization (Right set) = let s = states (toExplicit set)
                                 in ($s)

setNthElement :: [a] -> Int -> a -> [a]
setNthElement xs i val = fnt ++ val : bck
  where fnt = fst pair
        bck = tail (snd pair)
        pair = splitAt i xs

zipProperties :: [(String, Property)] -> [Test]
zipProperties = uncurry (zipWith testProperty) . unzip

zipTestCases :: [(String, Assertion)] -> [Test]
zipTestCases = uncurry (zipWith testCase) . unzip
