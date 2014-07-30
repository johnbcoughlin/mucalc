module MuCalc.MuModelTest (testList) where

import Data.Set
import Data.Map
import Data.Either (either)
import Control.Applicative
import MuCalc.MuFormula
import MuCalc.States
import MuCalc.MuModel
import Test.HUnit hiding (State)
import MuCalc.Generators

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testList = [ testProperty "proposition" propositionProperty
           , testProperty "negation" negationProperty
           , testProperty "disjunction" disjunctionProperty
           , testProperty "conjunction" conjunctionProperty
           , testCase "variable parity success" positiveVariableParityTest
           , testCase "variable parity failure" negativeVariableParityTest
           ]

implies p q = (not p) || q
iff p q = (p `implies` q) && (q `implies` p)
emptyContext = Data.Map.empty

propositionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model ->
                      forAll (dimNStates n) $ (\state ->
                      forAll (elements [0..n-1]) $ (\i ->
                      forAll (elements [True, False]) $ (\bool ->
                        let phi = Proposition i
                         in extract (realizeAux phi emptyContext model) (\set ->
                              whenFail' (putStrLn $ show set)
                                        ((set `contains` (setNthElement state i bool) `iff` bool))))))))

negationProperty = forAll dimensions $ (\n ->
                   forAll (models n) $ (\model ->
                   forAll (formulas n) $ (\phi ->
                     extract (realizeAux phi emptyContext model) (\phiSet ->
                     extract (realizeAux (Negation phi) emptyContext model) (\notPhiSet ->
                       property $ notPhiSet == setNot phiSet)))))

disjunctionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model -> let fn = (formulas n) in
                      forAll (pure (,) <*> fn <*> fn) $ (\(phi, psi) ->
                      forAll (dimNStates n) $ (\state ->
                        extract (realizeAux phi emptyContext model) (\phiSet ->
                        extract (realizeAux psi emptyContext model) (\psiSet ->
                        extract (realizeAux (Or phi psi) emptyContext model) (\unionSet ->
                          let prop1 = (phiSet `contains` state ==> unionSet `contains` state)
                              prop2 = (psiSet `contains` state ==> unionSet `contains` state)
                              prop3 = (unionSet `contains` state ==> phiSet `contains` state)
                              prop4 = (unionSet `contains` state ==> psiSet `contains` state)
                           in (prop1 .&&. prop2) .&&. (prop3 .||. prop4))))))))

conjunctionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model -> let fn = (formulas n) in
                      forAll (pure (,) <*> fn <*> fn) $ (\(phi, psi) ->
                      forAll (dimNStates n) $ (\state ->
                        extract (realizeAux phi emptyContext model) (\phiSet ->
                        extract (realizeAux psi emptyContext model) (\psiSet ->
                        extract (realizeAux (And phi psi) emptyContext model) (\intersectionSet ->
                          let prop1 = (phiSet `contains` state ==> intersectionSet `contains` state)
                              prop2 = (psiSet `contains` state ==> intersectionSet `contains` state)
                              prop3 = (intersectionSet `contains` state ==> phiSet `contains` state)
                              prop4 = (intersectionSet `contains` state ==> psiSet `contains` state)
                           in (prop1 .||. prop2) .&&. (prop3 .&&. prop4))))))))

--Chain these guys for maximum fun
extract :: Realization -> (StateSet -> Property) -> Property
extract (Left error) = const (property False) --ignore the given property computation
extract (Right set) = ($set) --apply the given computation to the set

aContext = Data.Map.fromList [("A", (newBottom 2, True))]
aModel = newMuModel 2

positiveVariableParityTest = let f = Or (Proposition 0)
                                        (Negation (And (Negation (Variable "A"))
                                                       (Proposition 1)))
                                 realization = realizeAux f aContext aModel
                              in assert (isRight realization)

negativeVariableParityTest = let fs = [ Negation (Variable "A")
                                      , Negation (Negation (Negation (Variable "A")))
                                      , Or (Proposition 0)
                                           (Negation (Variable "A"))
                                      ]
                                 realizations = Prelude.map (\f -> realizeAux f aContext aModel) fs
                              in assert (Prelude.all isLeft realizations)

isLeft = either (const True) (const False)
isRight = not . isLeft

setNthElement :: [a] -> Int -> a -> [a]
setNthElement xs i val = fnt ++ val : bck
  where fnt = fst pair
        bck = tail (snd pair)
        pair = splitAt i xs
