module MuCalc.MuModelTest (testList) where

import Data.Set
import Data.Map
import Control.Applicative
import MuCalc.MuFormula
import MuCalc.States
import MuCalc.MuModel
import Test.HUnit hiding (State)
import MuCalc.Generators

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testList = [ testProperty "proposition" propositionProperty
           , testProperty "negation" negationProperty
           , testProperty "disjunction" disjunctionProperty
           , testProperty "conjunction" conjunctionProperty
           ]

implies p q = (not p) || q
iff p q = (p `implies` q) && (q `implies` p)
emptyContext = Data.Map.empty

propositionProperty = forAll dimensions $ (\n ->
                      forAll (dimNStates n) $ (\state ->
                      forAll (elements [0..n-1]) $ (\i ->
                      forAll (elements [True, False]) $ (\bool ->
                        let model = newMuModel n
                            realization = realizeProposition i emptyContext model
                         in whenFail' (putStrLn $ show realization)
                                      ((realization `contains` (setNthElement state i bool) `iff` bool))))))

negationProperty = forAll dimensions $ (\n ->
                   forAll (models n) $ (\model ->
                   forAll (formulas n) $ (\phi ->
                     let notPhi = Negation phi
                         phiRealization = realize phi model
                         notPhiRealization = realize notPhi model
                      in notPhiRealization == setNot phiRealization)))

disjunctionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model -> let fn = (formulas n) in
                      forAll (pure (,) <*> fn <*> fn) $ (\(phi, psi) ->
                      forAll (dimNStates n) $ (\state ->
                        let phiSet = realize phi model
                            psiSet = realize psi model
                            unionSet = realize (Or phi psi) model
                            prop1 = (phiSet `contains` state ==> unionSet `contains` state)
                            prop2 = (psiSet `contains` state ==> unionSet `contains` state)
                            prop3 = (unionSet `contains` state ==> phiSet `contains` state)
                            prop4 = (unionSet `contains` state ==> psiSet `contains` state)
                         in (prop1 .&&. prop2) .&&. (prop3 .||. prop4)))))


conjunctionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model -> let fn = (formulas n) in
                      forAll (pure (,) <*> fn <*> fn) $ (\(phi, psi) ->
                      forAll (dimNStates n) $ (\state ->
                        let phiSet = realize phi model
                            psiSet = realize psi model
                            intersectionSet = realize (And phi psi) model
                            prop1 = (phiSet `contains` state ==> intersectionSet `contains` state)
                            prop2 = (psiSet `contains` state ==> intersectionSet `contains` state)
                            prop3 = (intersectionSet `contains` state ==> phiSet `contains` state)
                            prop4 = (intersectionSet `contains` state ==> psiSet `contains` state)
                         in (prop1 .||. prop2) .&&. (prop3 .&&. prop4)))))


setNthElement :: [a] -> Int -> a -> [a]
setNthElement xs i val = fnt ++ val : bck
  where fnt = fst pair
        bck = tail (snd pair)
        pair = splitAt i xs
