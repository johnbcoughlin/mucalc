module MuCalc.StatesTest (testList) where

import qualified Data.Set as S
import qualified Data.Map as M
import MuCalc.States
import MuCalc.Generators
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testList = [ testProperty "empty bottom" emptyBottom
           , testProperty "full top" fullTop
           , testProperty "singleton" singletonProp
           , testProperty "from explicit set property" fromExplicitProperty
           , testProperty "explicit bijections" explicitBijectionProperty
           , testProperty "Action property" actionProperty
           , testProperty "Action pullback property" actionPullbackProperty
           , testCase "map to state list" mapToStateListTest
           ]

emptyBottom = forAll dimensions (\n ->
              forAll (dimNStates n) (not . (newBottom `contains`)))

fullTop = forAll dimensions (\n ->
          forAll (dimNStates n) (newTop n `contains`))

singletonProp = forAllStates (\state -> (singleton state) `contains` state)

fromExplicitProperty = forAll dimensions (\n ->
                       forAll (listOf (dimNStates n)) (\states ->
                         let stateSet = fromExplicit states
                          in all (stateSet `contains`) states))

explicitBijectionProperty = forAll dimensions (\n ->
                            forAll (listOf (dimNStates n)) (\list ->
                              S.fromList list == S.fromList (toExplicit (fromExplicit list))))

--Expect the [output, input] pair of n-vector tuples
actionProperty = forAll dimensions (\n ->
                   let domain = enumerateStates n
                    in forAll (iffActions n) (\f ->
                       forAll (dimNStates n) (\state ->
                         let action = fromFunction domain f
                             expected = map (++state) (f state)
                          --Verify that the action set contains every expected value.
                          in all (action `contains`) expected)))

--Check that the action pullback function pulls back to states from which we can reach the goal image.
actionPullbackProperty = forAll dimensions (\n ->
                           let domain = enumerateStates n
                            in forAll (iffActions n) (\f ->
                               forAll (listOf (dimNStates n)) (\stateList ->
                                 let action = fromFunction domain f
                                     image = fromExplicit stateList
                                     preImage = toExplicit $ throughAction image action
                                     hasAnyFResultsInImage preImageState = any (image `contains`) (f preImageState)
                                  in all hasAnyFResultsInImage preImage)))

--TODO: rewrite as the property that the result should have 2^|free variables| elements.
mapToStateListTest = let hash = M.fromList [(4, True), (6, False)]
                         expected = [ [True, True, False, True]
                                    , [True, True, False, False]
                                    , [True, False, False, True]
                                    , [True, False, False, False] ]
                      in S.fromList (rebaseMapToState 4 hash) @?= S.fromList expected
