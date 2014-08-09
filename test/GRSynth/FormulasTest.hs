module GRSynth.FormulasTest ( model
                            , testList
                            ) where

import Test.Framework
import Test.HUnit hiding (State)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import GRSynth.Formulas
import GRSynth.States
import qualified Data.Map as M

testList = [ testCase "All false" allFalseTest
           , testCase "Exactly one" exactlyOneTest
           , testCase "Exactly two" exactlyTwoTest
           , testCase "All true" allTrueTest
           , testCase "Unknown atomic proposition" unknownAtomTest
           ]

-- setup
domain = enumerateStates 3
set0, set1, set2 :: StateSet
set0 = fromExplicit $ filter (!!0) domain
set1 = fromExplicit $ filter (!!1) domain
set2 = fromExplicit $ filter (!!2) domain

model = M.fromList [("0", set0), ("1", set1), ("2", set2)]

allFalse, exactlyOneTrue, exactlyTwoTrue, allTrue :: SimpleFormula
allFalse = And (Negation (Atom "0"))
          (And (Negation (Atom "1"))
               (Negation (Atom "2")))
exactlyOneTrue = Or (And (Atom "0") (And (Negation (Atom "1")) (Negation (Atom "2"))))
                (Or (And (Atom "1") (And (Negation (Atom "0")) (Negation (Atom "2"))))
                    (And (Atom "2") (And (Negation (Atom "0")) (Negation (Atom "1")))))
exactlyTwoTrue = Or (And (Atom "0") (And (Atom "1") (Negation (Atom "2"))))
                (Or (And (Atom "1") (And (Atom "2") (Negation (Atom "0"))))
                    (And (Atom "2") (And (Atom "0") (Negation (Atom "1")))))
allTrue = And (Atom "0")
         (And (Atom "1")
              (Atom "2"))

allFalseTest = assertRealization (realize model allFalse) (\list ->
               list @?= [[False, False, False]])

exactlyOneTest = assertRealization (realize model exactlyOneTrue) (\list ->
                 list @?= [[True, False, False], [False, True, False], [False, False, True]])

exactlyTwoTest = assertRealization (realize model exactlyTwoTrue) (\list ->
                 list @?= [[True, True, False], [True, False, True], [False, True, True]])

allTrueTest = assertRealization (realize model allTrue) (\list ->
              list @?= [[True, True, True]])

unknownAtomTest = (realize model (And (Atom "0") (Atom "foo")))
                  @?= Left UnknownAtomError

assertRealization :: Realization -> ([PState] -> Assertion) -> Assertion
assertRealization (Left s) f = error $ show s
assertRealization (Right set) f = f $ toExplicit set
