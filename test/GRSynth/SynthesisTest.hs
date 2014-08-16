module GRSynth.SynthesisTest where

import Test.Framework
import Test.HUnit hiding (State, Test)
import Test.Framework.Providers.HUnit
import GRSynth.Utils
import GRSynth.GameStructure
import GRSynth.Formulas
import GRSynth.States
import GRSynth.Semantics
import GRSynth.Synthesis

testList = [ testGroup "Cox" coxTests
           , testGroup "Fixpoint" fixpointStopTests
           , testGroup "Synthesis" winningPositionTests
           ]

type S = (Int2, Int2)

--We really only need three atomic propositions: x == 0, y == 3, and the initial condition
liveness :: S -> Bool
liveness (Int2 0, _) = True
liveness _ = False

winning :: S -> Bool
winning (_, Int2 3) = True
winning _ = False

initial :: [S]
initial = [(Int2 0, Int2 0)]

--The environment may cycle the x state by 1
envAction :: S -> [Int2]
envAction (Int2 x, _) = [Int2 x, Int2 ((x+1) `mod` 4)]

--The system may cycle the y state by 1 if x == 0.
sysAction :: S -> [S]
sysAction (Int2 0, Int2 y) = [(Int2 0, Int2 y), (Int2 0, Int2 ((y+1) `mod` 4))]
sysAction s = [s]

--Construct the game structure
gs :: GS Int2 Int2
gs = (((baseGS `withPropAsPredicate` "liveness") liveness
               `withPropAsPredicate` "winning") winning
               `withPropAsSupport` "initial") initial
               `withInitialState` (Atom "initial")
               `withAssumption` (Atom "liveness")
               `withGuarantee` (Atom "winning")
               `withEnvActionAsFunction` envAction
               `withSysActionAsFunction` sysAction

coxTest :: [S] -> [S] -> Assertion
coxTest phi expected = let set = fromExplicit (map encode phi)
                           pStates = toExplicit (cox (gsenv gs) (gssys gs) set)
                        in interpret pStates @?~ expected

coxTests = [ testCase "Impossible position" $ coxTest [(Int2 1, Int2 1)] ([]::[S])
           , testCase "Forceable" $ coxTest [(Int2 0, Int2 2), (Int2 1, Int2 1)] [(Int2 0, Int2 1)]
           ]

fixpointStopTests = [ testCase "Bottom < Top" bottomTopTest
                    , testCase "Top < Bottom" topBottomTest
                    , testCase "Top <> Top" topTopTest
                    , testCase "Bottom <> Bottom" bottomBottomTest
                    , testCase "Bottom < Something" bottomSomethingTest
                    , testCase "Something < ManyThings" somethingManyThingsTest
                    , testCase "ManyThings < Top" manyThingsTopTest
                    ]

something :: StateSet
something = fromExplicit [[False, False]]

manyThings :: StateSet
manyThings = fromExplicit [[False, False], [False, True], [True, False]]

bottomTopTest = fixpointStop newBottom newTop @?= False
topBottomTest = fixpointStop newTop newBottom @?= True
topTopTest = fixpointStop newTop newTop @?= True
bottomBottomTest = fixpointStop newBottom newBottom @?= True
bottomSomethingTest = fixpointStop newBottom something @?= False
somethingManyThingsTest = fixpointStop something manyThings @?= False
manyThingsTopTest = fixpointStop manyThings newTop @?= False

winningPositionTests = [ testCase "Winning from everywhere" winningEverywhereTest
                       , testCase "Winning from nowhere" winningNowhereTest
                       , testCase "Winning from somewhere" winningSomewhereTest
                       ]

winningEverywhereTest = let result = toExplicit $ synth (finalize gs)
                         in interpret result @?~ cartProd

winningNowhereTest = let winning = [(Int2 2, Int2 3)]
                         newGS = finalize $ (gs `withPropAsSupport` "winning") winning
                         result = toExplicit $ synth newGS
                      in interpret result @?~ ([]::[S])

{-
- In addition to the obviously winning conditions where y == 3 already, it's possible to win
- from (2, 2) and (3, 2) because the environment has to pass through 0 once on its way
- to the liveness condition at (1, _).
-}
winningSomewhereTest = let liveness (Int2 1, _) = True
                           liveness _ = False
                           newGS = finalize $ (gs `withPropAsPredicate` "liveness") liveness
                           result = toExplicit $ synth newGS
                        in interpret result @?~
                        [(Int2 0, Int2 3), (Int2 1, Int2 3), (Int2 2, Int2 3), (Int2 3, Int2 3),
                         (Int2 2, Int2 2), (Int2 3, Int2 2)]

{-
- If we have two liveness conditions at (1, _) and (2, _) then the system must cycle around to
- satisfy them, visiting (0, _) infinitely often. Thus every position is winning.
-}
winningEverywhereTest2 = let liveness (Int2 1, _) = True
                             liveness _ = False
                             liveness2 (Int2 2, _) = True
                             liveness2 _ = False
                             phi = (Atom "liveness2")
                             newGS = finalize $ ((gs `withPropAsPredicate` "liveness") liveness
                                                     `withPropAsPredicate` "liveness2") liveness2
                                                     `withAssumption` phi
                             result = toExplicit $ synth newGS
                          in interpret result @?~ cartProd


