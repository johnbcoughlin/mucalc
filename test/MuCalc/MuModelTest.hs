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

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testList = [ testGroup "Formula Properties" formulaProperties
           , testCase "variable parity success" positiveVariableParityTest
           , testCase "variable parity failure" negativeVariableParityTest
           , testGroup "setOneTrue" setOneTrueTests
           , testCase "allFalse" allFalseTest
           , testCase "exactlyOneTrue" exactlyOneTrueTest
           , testCase "allTrue" allTrueTest
           , testCase "allFalseReachable" allFalseReachableTest
           , testCase "exactlyOneTrueReachable" exactlyOneTrueReachableTest
           , testProperty "least fixpoint" leastFixpointProperty
           ]

formulaProperties = zipWith testProperty ["proposition", "negation", "disjunction", "conjunction"]
                                         [propositionProperty, negationProperty, disjunctionProperty, conjunctionProperty]
propositionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model ->
                      forAll (dimNStates n) $ (\state ->
                      forAll (elements [0..n-1]) $ (\i ->
                      forAll (elements [True, False]) $ (\bool ->
                        let phi = Proposition i
                         in extract (realize phi model) (\set ->
                              whenFail' (putStrLn $ show set)
                                        ((set `contains` (setNthElement state i bool) `iff` bool))))))))

negationProperty = forAllModels $ (\(model, formulas, _) ->
                   forAll (formulas) $ (\phi ->
                     extract (realize phi model) (\phiSet ->
                     extract (realize (Negation phi) model) (\notPhiSet ->
                       property $ notPhiSet == setNot phiSet))))

disjunctionProperty = forAllModels $ (\(model, formulas, _) ->
                      forAll (pairsOf formulas) $ (\(phi, psi) ->
                        extract (realize phi model) (\phiSet ->
                        extract (realize psi model) (\psiSet ->
                        extract (realize (Or phi psi) model) (\unionSet ->
                          let subset = subsetN $ dimension model
                           in phiSet `subset` unionSet .&&.
                              psiSet `subset` unionSet .&&.
                              unionSet `subset` setOr phiSet psiSet)))))

conjunctionProperty = forAllModels $ (\(model, formulas, _) ->
                      forAll (pairsOf formulas) $ (\(phi, psi) ->
                        extract (realize phi model) (\phiSet ->
                        extract (realize psi model) (\psiSet ->
                        extract (realize (And phi psi) model) (\intersectionSet ->
                          let subset = subsetN $ dimension model
                           in intersectionSet `subset` phiSet .&&.
                              intersectionSet `subset` psiSet .&&.
                              setAnd phiSet psiSet `subset` intersectionSet)))))



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
                in S.fromList (s:list)

setOneTrueTests = zipWith testCase ["true-false", "true-true", "false-false"]
                                   [s1Test,       s2Test,      s3Test]
s1Test = setOneTrue [True, False] @?= S.fromList [[True, False], [True, True]]
s2Test = setOneTrue [True, True] @?= S.fromList [[True, True]]
s3Test = setOneTrue [False, False] @?= S.fromList [[False, False], [True, False], [False, True]]

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
exactlyOneTrueList = [[True, False, False] , [False, True, False] , [False, False, True]]
allTrue = And (Proposition 0)
         (And (Proposition 1)
              (Proposition 2))

--Test that we've written these formulas correctly
allFalseTest = assertRealization (realize allFalse m) $ (\set ->
                 assert (set == S.fromList [[False, False, False]]))

exactlyOneTrueTest = assertRealization (realize exactlyOneTrue m) $ (\set ->
                       assert (set == S.fromList exactlyOneTrueList))

allTrueTest = assertRealization (realize allTrue m) $ (\set ->
                assert (set == S.fromList [[True, True, True]]))

--Test the reachability of the states through the transition
allFalseReachableTest = let phi = PossiblyNext "setOneTrue" allFalse
                           in assertRealization (realize phi m) $ (\set ->
                                assert (set == S.fromList [[False, False, False]]))

exactlyOneTrueReachableTest = let phi = PossiblyNext "setOneTrue" exactlyOneTrue
                               in assertRealization (realize phi m) $ (\set ->
                                    assert (set == S.fromList ([False, False, False] : exactlyOneTrueList)))

leastFixpointProperty = property True
