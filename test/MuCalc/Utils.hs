module MuCalc.Utils where

import Data.Either (either)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import MuCalc.MuFormula hiding (iff, implies)
import MuCalc.States
import MuCalc.Realization
import GRSynth.Semantics
import Test.QuickCheck
import Test.HUnit hiding (Test, State)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Framework

isLeft = either (const True) (const False)
isRight = not . isLeft

implies p q = (not p) || q
iff p q = (p `implies` q) && (q `implies` p)

zipProperties :: [(String, Property)] -> [Test]
zipProperties = uncurry (zipWith testProperty) . unzip

zipTestCases :: [(String, Assertion)] -> [Test]
zipTestCases = uncurry (zipWith testCase) . unzip

interpret :: State s => [PState] -> [s]
interpret = map (fst . fromJust . decode)

(@?~) :: (Ord a, Show a) => [a] -> [a] -> Assertion
(@?~) xs ys = S.fromList xs @?= S.fromList ys
