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
gs :: GameStructure Int2 Int2
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
                           pStates = toExplicit (cox (env gs) (sys gs) set)
                        in interpret pStates @?~ expected

coxTests = [ testCase "Impossible position" $ coxTest [(Int2 1, Int2 1)] ([]::[S])
           , testCase "Forceable" $ coxTest [(Int2 0, Int2 2), (Int2 1, Int2 1)] [(Int2 0, Int2 1)]
           ]

