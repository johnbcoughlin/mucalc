module MuCalc.Utils where

import Data.Either (either)
import qualified Data.Set as S
import MuCalc.MuFormula
import MuCalc.States
import MuCalc.MuModel
import Test.QuickCheck
import Test.HUnit hiding (State)

isLeft = either (const True) (const False)
isRight = not . isLeft

implies p q = (not p) || q
iff p q = (p `implies` q) && (q `implies` p)

--Chain these guys for maximum fun
extract :: Realization -> (StateSet -> Property) -> Property
extract (Left error) = const (property False) --ignore the given property computation
extract (Right set) = ($set) --apply the given computation to the set

assertRealization :: Realization -> (S.Set State -> Assertion) -> Assertion
assertRealization (Left error) = const (assert False)
assertRealization (Right s) = let set = states (toExplicit s)
                               in ($set)

setNthElement :: [a] -> Int -> a -> [a]
setNthElement xs i val = fnt ++ val : bck
  where fnt = fst pair
        bck = tail (snd pair)
        pair = splitAt i xs


