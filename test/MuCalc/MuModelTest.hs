module MuCalc.MuModelTest (testList) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import MuCalc.MuFormula
import MuCalc.States
import MuCalc.MuModel
import MuCalc.Utils
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
           , testCase "setOneTrue" setOneTrueTest
           , testCase "allFalse" allFalseTest
           , testCase "exactlyOneTrue" exactlyOneTrueTest
           , testCase "allTrue" allTrueTest
           , testCase "allFalseUnreachable" allFalseUnreachableTest
           , testCase "exactlyOneTrueReachable" exactlyOneTrueReachableTest
           ]

propositionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model ->
                      forAll (dimNStates n) $ (\state ->
                      forAll (elements [0..n-1]) $ (\i ->
                      forAll (elements [True, False]) $ (\bool ->
                        let phi = Proposition i
                         in extract (realize phi model) (\set ->
                              whenFail' (putStrLn $ show set)
                                        ((set `contains` (setNthElement state i bool) `iff` bool))))))))

negationProperty = forAll dimensions $ (\n ->
                   forAll (models n) $ (\model ->
                   forAll (formulas n) $ (\phi ->
                     extract (realize phi model) (\phiSet ->
                     extract (realize (Negation phi) model) (\notPhiSet ->
                       property $ notPhiSet == setNot phiSet)))))

disjunctionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model -> let fn = (formulas n) in
                      forAll (pure (,) <*> fn <*> fn) $ (\(phi, psi) ->
                      forAll (dimNStates n) $ (\state ->
                        extract (realize phi model) (\phiSet ->
                        extract (realize psi model) (\psiSet ->
                        extract (realize (Or phi psi) model) (\unionSet ->
                          let prop1 = (phiSet `contains` state ==> unionSet `contains` state)
                              prop2 = (psiSet `contains` state ==> unionSet `contains` state)
                              prop3 = (unionSet `contains` state ==> phiSet `contains` state)
                              prop4 = (unionSet `contains` state ==> psiSet `contains` state)
                           in (prop1 .&&. prop2) .&&. (prop3 .||. prop4))))))))

conjunctionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model -> let fn = (formulas n) in
                      forAll (pure (,) <*> fn <*> fn) $ (\(phi, psi) ->
                      forAll (dimNStates n) $ (\state ->
                        extract (realize phi model) (\phiSet ->
                        extract (realize psi model) (\psiSet ->
                        extract (realize (And phi psi) model) (\intersectionSet ->
                          let prop1 = (phiSet `contains` state ==> intersectionSet `contains` state)
                              prop2 = (psiSet `contains` state ==> intersectionSet `contains` state)
                              prop3 = (intersectionSet `contains` state ==> phiSet `contains` state)
                              prop4 = (intersectionSet `contains` state ==> psiSet `contains` state)
                           in (prop1 .||. prop2) .&&. (prop3 .&&. prop4))))))))

aContext = M.fromList [("A", (newBottom 2, True))]
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
                                 realizations = map (\f -> realizeAux f aContext aModel) fs
                              in assert (all isLeft realizations)

-- Tests for transition realization --

setOneTrue :: State -> S.Set State
setOneTrue s = let n = length s
                   f = \i -> if not (s !! i)
                             then [setNthElement s i True]
                             else []
                   list = concatMap f [0..n-1]
                in S.fromList list

setOneTrueTest = let s1Test = setOneTrue [True, False] == S.fromList [[True, True]]
                     s2Test = setOneTrue [True, True] == S.fromList []
                     s3Test = setOneTrue [False, False] == S.fromList [[True, False], [False, True]]
                  in assert (s1Test && s2Test && s3Test)

explicitOfN :: Int -> S.Set State -> ExplicitStateSet
explicitOfN n set = Explicit set n

dim = 3
setOneTrueTransition = fromFunction dim ((explicitOfN dim) . setOneTrue)
m = (newMuModel dim) { transitions = M.fromList [("setOneTrue", setOneTrueTransition)] }
allFalse = And (Negation (Proposition 0))
          (And (Negation (Proposition 1))
               (Negation (Proposition 2)))
exactlyOneTrue = Or (And (Proposition 0) (And (Negation (Proposition 1)) (Negation (Proposition 2))))
                (Or (And (Proposition 1) (And (Negation (Proposition 0)) (Negation (Proposition 2))))
                    (And (Proposition 2) (And (Negation (Proposition 0)) (Negation (Proposition 1)))))
allTrue = And (Proposition 0)
         (And (Proposition 1)
              (Proposition 2))

--Test that we've written these formulas correctly
allFalseTest = assertRealization (realize allFalse m) $ (\set ->
                 assert (set == S.fromList [[False, False, False]]))

exactlyOneTrueTest = assertRealization (realize exactlyOneTrue m) $ (\set ->
                       assert (set == S.fromList [ [True, False, False]
                                                 , [False, True, False]
                                                 , [False, False, True]
                                                 ]))

allTrueTest = assertRealization (realize allTrue m) $ (\set ->
                assert (set == S.fromList [[True, True, True]]))

--Test the reachability of the states through the transition
allFalseUnreachableTest = let phi = PossiblyNext "setOneTrue" allFalse
                           in assertRealization (realize phi m) $ (\set ->
                                assert (S.null set))

exactlyOneTrueReachableTest = let phi = PossiblyNext "setOneTrue" exactlyOneTrue
                               in assertRealization (realize phi m) $ (\set ->
                                    assert (set == S.fromList [[False, False, False]]))


