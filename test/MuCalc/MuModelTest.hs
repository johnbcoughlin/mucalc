{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Maybe

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
           ]

-- | State space of boolean 3-vectors
instance State PState where
  encode = id
  decode = Just . id

aContext = Context True (M.singleton "A" newBottom)
aModel = let base = newMuModel (enumerateStates 2)
             m1 = withProp base "0" (!!0)
             m2 = withProp m1 "1" (!!1)
          in m2

variableParityTests = zipTestCases [ ("Positive", positiveVariableParityTest)
                                   , ("Negative", negativeVariableParityTest)
                                   ]
positiveVariableParityTest = let f = Or (Atom "0")
                                        (Negation (And (Negation (Variable "A"))
                                                       (Atom "1")))
                                 realization = realizeAux f aContext aModel
                              in assert (isRight realization)

negativeVariableParityTest = let fs = [ Negation (Variable "A")
                                      , Negation (Negation (Negation (Variable "A")))
                                      , Or (Atom "0")
                                           (Negation (Variable "A"))
                                      ]
                                 realizations = map (\f -> realizeAux f aContext aModel) fs
                              in assert (all isLeft realizations)

-- Tests for action realization --

--Either set exactly one element true, or change nothing.
setOneTrue :: PState -> [PState]
setOneTrue s = let n = length s
                   f i = [setNthElement s i True | not (s !! i)]
                   list = concatMap f [0..n-1]
                in s:list

setOneTrueTests = zipTestCases [ ("true-false", s1Test)
                               , ("true-true", s2Test)
                               , ("false-false", s3Test)
                               ]
s1Test = setOneTrue [True, False] @?= [[True, False], [True, True]]
s2Test = setOneTrue [True, True] @?= [[True, True]]
s3Test = setOneTrue [False, False] @?= [[False, False], [True, False], [False, True]]

n = 3
m = let base = newMuModel (enumerateStates 3)
        m1 = withAction base "setOneTrue" setOneTrue
        m2 = withProp m1 "0" (!!0)
        m3 = withProp m2 "1" (!!1)
        m4 = withProp m3 "2" (!!2)
     in m4

allFalse = And (Negation (Atom "0"))
          (And (Negation (Atom "1"))
               (Negation (Atom "2")))
exactlyOneTrue = Or (And (Atom "0") (And (Negation (Atom "1")) (Negation (Atom "2"))))
                (Or (And (Atom "1") (And (Negation (Atom "0")) (Negation (Atom "2"))))
                    (And (Atom "2") (And (Negation (Atom "0")) (Negation (Atom "1")))))
exactlyOneTrueList = [[True, False, False] , [False, True, False] , [False, False, True]]
exactlyTwoTrue = Or (And (Atom "0") (And (Atom "1") (Negation (Atom "2"))))
                (Or (And (Atom "1") (And (Atom "2") (Negation (Atom "0"))))
                    (And (Atom "2") (And (Atom "0") (Negation (Atom "1")))))
exactlyTwoTrueList = [[True, True, False] , [True, False, True] , [False, True, True]]
allTrue = And (Atom "0")
         (And (Atom "1")
              (Atom "2"))

--Test that we've written these formulas correctly
testFormulaTests = zipTestCases [ ("All false", allFalseTest)
                                , ("Exactly one true", exactlyOneTrueTest)
                                , ("Exactly two true", exactlyTwoTrueTest)
                                , ("All true", allTrueTest)
                                ]

allFalseTest = assertRealization (realize allFalse m) (\list ->
                 list @?= [[False, False, False]])
exactlyOneTrueTest = assertRealization (realize exactlyOneTrue m) (\list ->
                       list @?= exactlyOneTrueList)
exactlyTwoTrueTest = assertRealization (realize exactlyTwoTrue m) (\list ->
                       list @?= exactlyTwoTrueList)
allTrueTest = assertRealization (realize allTrue m) (\list ->
                list @?= [[True, True, True]])

--Test the reachability of the states through the action
allFalseReachableTest = let phi = PossiblyNext "setOneTrue" allFalse
                           in assertRealization (realize phi m) (\list ->
                                list @?= [[False, False, False]])

exactlyOneTrueReachableTest = let phi = PossiblyNext "setOneTrue" exactlyOneTrue
                               in assertRealization (realize phi m) (\list ->
                                    list @?= ([False, False, False] : exactlyOneTrueList))

muTestCases = zipTestCases [ ("Least fixpoint of constant", constantFixpointTest)
                           , ("Least fixpoint of disjunction", simpleOrFixpointTest)
                           , ("Least fixpoint of conjunction", simpleAndFixpointTest)
                           , ("Unbound variable check", unboundVariableCheck)
                           , ("Exactly two true fixpoint", exactlyTwoTrueFixpointTest)
                           , ("Exactly one true fixpoint", exactlyOneTrueFixpointTest)
                           ]

--The least fixpoint of a constant function should be the constant
constantFixpointTest = let phi = Mu "A" exactlyOneTrue
                        in assertRealization (realize phi m) (\list ->
                             list @?= exactlyOneTrueList)

--The least fixpoint of \Z -> Z || phi should be phi
simpleOrFixpointTest = let phi = Mu "A" (Or (exactlyOneTrue) (Variable "A"))
                        in assertRealization (realize phi m) (\list ->
                             list @?= exactlyOneTrueList)

simpleAndFixpointTest = let phi = Mu "A" (And (exactlyOneTrue) (Variable "A"))
                         in assertRealization (realize phi m) (\list ->
                              list @?= [])

unboundVariableCheck = let phi = Mu "A" (And (exactlyOneTrue) (Variable "B"))
                        in (realize phi m) @?= Left UnknownVariableError

--These tests rely on the fact that the setOneTrue action only allows you to move
--up the lattice, so that exactlyOneTrue is not reachable from exactlyTwoTrue, and
--exactlyTwoTrue is not reachable from allTrue.
exactlyTwoTrueFixpointTest = let phi = Mu "A" (Or (exactlyTwoTrue)
                                                  (PossiblyNext "setOneTrue" (Variable "A")))
                                 notAllTrue = Negation allTrue
                              in assertRealization (realize phi m) (\set ->
                                 assertRealization (realize notAllTrue m) (\expected ->
                                   set @?= expected))

exactlyOneTrueFixpointTest = let phi = Mu "A" (Or (exactlyOneTrue)
                                                  (PossiblyNext "setOneTrue" (Variable "A")))
                                 atMostOneTrue = Negation (Or allTrue exactlyTwoTrue)
                              in assertRealization (realize phi m) (\set ->
                                 assertRealization (realize atMostOneTrue m) (\expected ->
                                   set @?= expected))

