module MuCalc.MuModelTest (testList) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import MuCalc.MuFormula
import MuCalc.States
import MuCalc.MuModel
import MuCalc.Realization
import MuCalc.Utils
import Test.HUnit hiding (State)
import MuCalc.Generators
import MuCalc.MuModelProperties (formulaProperties)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testList = [ testGroup "Variable parity" variableParityTests
           , testGroup "setOneTrue" setOneTrueTests
           , testGroup "Test formulas" testFormulaTests
           , testCase "allFalseReachable" allFalseReachableTest
           , testCase "exactlyOneTrueReachable" exactlyOneTrueReachableTest
           , testGroup "Fixpoint tests" muTestCases
           , testGroup "Formula Properties" formulaProperties
           ]

aContext = Context True (M.singleton "A" (newBottom 2))
aModel = newMuModel 2

variableParityTests = zipTestCases [ ("Positive", positiveVariableParityTest)
                                   , ("Negative", negativeVariableParityTest)
                                   ]
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

setOneTrueTests = zipTestCases [ ("true-false", s1Test)
                               , ("true-true", s2Test)
                               , ("false-false", s3Test)
                               ]
s1Test = setOneTrue [True, False] @?= S.fromList [[True, False], [True, True]]
s2Test = setOneTrue [True, True] @?= S.fromList [[True, True]]
s3Test = setOneTrue [False, False] @?= S.fromList [[False, False], [True, False], [False, True]]

explicitOfN :: Int -> S.Set State -> ExplicitStateSet
explicitOfN n set = Explicit set n

n = 3
setOneTrueTransition = fromFunction n ((explicitOfN n) . setOneTrue)
m = (newMuModel n) { transitions = M.fromList [("setOneTrue", setOneTrueTransition)] }
allFalse = And (Negation (Proposition 0))
          (And (Negation (Proposition 1))
               (Negation (Proposition 2)))
exactlyOneTrue = Or (And (Proposition 0) (And (Negation (Proposition 1)) (Negation (Proposition 2))))
                (Or (And (Proposition 1) (And (Negation (Proposition 0)) (Negation (Proposition 2))))
                    (And (Proposition 2) (And (Negation (Proposition 0)) (Negation (Proposition 1)))))
exactlyOneTrueList = [[True, False, False] , [False, True, False] , [False, False, True]]
exactlyTwoTrue = Or (And (Proposition 0) (And (Proposition 1) (Negation (Proposition 2))))
                (Or (And (Proposition 1) (And (Proposition 2) (Negation (Proposition 0))))
                    (And (Proposition 2) (And (Proposition 0) (Negation (Proposition 1)))))
exactlyTwoTrueList = [[True, True, False] , [True, False, True] , [False, True, True]]
allTrue = And (Proposition 0)
         (And (Proposition 1)
              (Proposition 2))

--Test that we've written these formulas correctly
testFormulaTests = zipTestCases [ ("All false", allFalseTest)
                                , ("Exactly one true", exactlyOneTrueTest)
                                , ("Exactly two true", exactlyTwoTrueTest)
                                , ("All true", allTrueTest)
                                ]

allFalseTest = assertRealization (realize allFalse m) $ (\set ->
                 set @?= S.fromList [[False, False, False]])
exactlyOneTrueTest = assertRealization (realize exactlyOneTrue m) $ (\set ->
                       set @?= S.fromList exactlyOneTrueList)
exactlyTwoTrueTest = assertRealization (realize exactlyTwoTrue m) $ (\set ->
                       set @?= S.fromList exactlyTwoTrueList)
allTrueTest = assertRealization (realize allTrue m) $ (\set ->
                set @?= S.fromList [[True, True, True]])

--Test the reachability of the states through the transition
allFalseReachableTest = let phi = PossiblyNext "setOneTrue" allFalse
                           in assertRealization (realize phi m) $ (\set ->
                                set @?= S.fromList [[False, False, False]])

exactlyOneTrueReachableTest = let phi = PossiblyNext "setOneTrue" exactlyOneTrue
                               in assertRealization (realize phi m) $ (\set ->
                                    set @?= S.fromList ([False, False, False] : exactlyOneTrueList))

muTestCases = zipTestCases [ ("Least fixpoint of constant", constantFixpointTest)
                           , ("Least fixpoint of disjunction", simpleOrFixpointTest)
                           , ("Least fixpoint of conjunction", simpleAndFixpointTest)
                           , ("Unbound variable check", unboundVariableCheck)
                           , ("Exactly two true fixpoint", exactlyTwoTrueFixpointTest)
                           , ("Exactly one true fixpoint", exactlyOneTrueFixpointTest)
                           ]

--The least fixpoint of a constant function should be the constant
constantFixpointTest = let phi = Mu "A" exactlyOneTrue
                        in assertRealization (realize phi m) $ (\set ->
                             set @?= S.fromList exactlyOneTrueList)

--The least fixpoint of \Z -> Z || phi should be phi
simpleOrFixpointTest = let phi = Mu "A" (Or (exactlyOneTrue) (Variable "A"))
                        in assertRealization (realize phi m) $ (\set ->
                             set @?= S.fromList exactlyOneTrueList)

simpleAndFixpointTest = let phi = Mu "A" (And (exactlyOneTrue) (Variable "A"))
                         in assertRealization (realize phi m) $ (\set ->
                              set @?= S.fromList [])

unboundVariableCheck = let phi = Mu "A" (And (exactlyOneTrue) (Variable "B"))
                        in (realize phi m) @?= Left UnknownVariableError

--These tests rely on the fact that the setOneTrue transition only allows you to move
--up the lattice, so that exactlyOneTrue is not reachable from exactlyTwoTrue, and
--exactlyTwoTrue is not reachable from allTrue.
exactlyTwoTrueFixpointTest = let phi = Mu "A" (Or (exactlyTwoTrue)
                                                  (PossiblyNext "setOneTrue" (Variable "A")))
                                 notAllTrue = Negation allTrue
                              in assertRealization (realize phi m) $ (\set ->
                                 assertRealization (realize notAllTrue m) $ (\expected ->
                                   set @?= expected))

exactlyOneTrueFixpointTest = let phi = Mu "A" (Or (exactlyOneTrue)
                                                  (PossiblyNext "setOneTrue" (Variable "A")))
                                 atMostOneTrue = Negation (Or allTrue exactlyTwoTrue)
                              in assertRealization (realize phi m) $ (\set ->
                                 assertRealization (realize atMostOneTrue m) $ (\expected ->
                                   set @?= expected))

