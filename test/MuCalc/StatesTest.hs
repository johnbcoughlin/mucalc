module MuCalc.StatesTest (testList) where

import qualified Data.Set as S
import qualified Data.Map as M
import MuCalc.States
import MuCalc.Generators
import Test.HUnit hiding (State)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testList = [ testProperty "empty bottom" emptyBottom
           , testProperty "full top" fullTop
           , testProperty "singleton" singletonProp
           , testProperty "from explicit set property" fromExplicitProperty
           , testProperty "explicit bijections" explicitBijectionProperty
           , testProperty "transition property" transitionProperty
           , testProperty "transition pullback property" transitionPullbackProperty
           , testCase "map to state list" mapToStateListTest
           ]

emptyBottom = forAll dimensions $ (\n ->
              forAll (dimNStates n) $ (not . (newBottom n `contains`)))

fullTop = forAll dimensions $ (\n ->
          forAll (dimNStates n) $ (newTop n `contains`))

singletonProp = forAllStates $ (\state -> (singleton state) `contains` state)

fromExplicitProperty = forAll dimensions $ (\n ->
                       forAll (listOf (dimNStates n)) $ (\states ->
                         let stateSet = fromExplicit (Explicit (S.fromList states) n)
                          in all (stateSet `contains`) states))

explicitBijectionProperty = forAll dimensions $ (\n ->
                            forAll (listOf (dimNStates n)) $ (\list ->
                              let explicit = (Explicit (S.fromList list) n)
                               in explicit == toExplicit (fromExplicit explicit)))

--Expect the [output, input] pair of n-vector tuples
transitionProperty = forAll dimensions $ (\n ->
                     forAll (iffTransitions n) $ (\f ->
                     forAll (dimNStates n) $ (\state ->
                       let transition = fanoutToPhysicalTransition n f
                           expected = map (++state) (f state)
                        --Verify that the transition set contains every expected value.
                        in all (transition `contains`) expected)))

--Check that the transition pullback function pulls back to states from which we can reach the goal image.
transitionPullbackProperty = forAll dimensions $ (\n ->
                             forAll (iffTransitions n) $ (\f ->
                             forAll (listOf (dimNStates n)) $ (\stateList ->
                               let transition = fanoutToPhysicalTransition n f
                                   image = fromExplicit (Explicit (S.fromList stateList) n)
                                   preImage = S.elems . states . toExplicit $ throughTransition image transition
                                   hasAnyFResultsInImage = (\preImageState ->
                                                           any (image `contains`) (f $ preImageState))
                                in all hasAnyFResultsInImage preImage)))

--TODO: rewrite as the property that the result should have 2^|free variables| elements.
mapToStateListTest = let hash = M.fromList [(4, True), (6, False)]
                         expected = [ [True, True, False, True]
                                    , [True, True, False, False]
                                    , [True, False, False, True]
                                    , [True, False, False, False] ]
                      in S.fromList (rebaseMapToState 4 hash) @?= S.fromList expected
