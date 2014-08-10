module GRSynth.Utils where

import Data.Either (either)
import Data.Maybe (fromJust)
import Control.Applicative
import qualified Data.Set as S
import MuCalc.MuFormula hiding (iff, implies)
import GRSynth.States
import MuCalc.Realization
import GRSynth.Semantics
import GRSynth.GameStructure
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

newtype Int2 = Int2 Int
               deriving (Show, Eq, Ord)

instance State Int2 where
  encode (Int2 x) = if x < 0 || x >= 4
                    then error "Invalid 2 bit integer"
                    else [x >= 2, x `mod` 2 == 1]
  decode pState = if length pState < 2
                  then Nothing
                  else let (two, rest) = splitAt 2 pState
                           x = (if head two then 2 else 0) + (if head $ tail two then 1 else 0)
                        in Just (Int2 x, rest)

domX = map Int2 [0..3]
domY = map Int2 [0..3]
cartProd = (,) <$> domX <*> domY
baseGS = newGameStructure domX domY
