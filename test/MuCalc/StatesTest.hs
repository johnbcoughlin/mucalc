{-# LANGUAGE FlexibleInstances #-}

module MuCalc.StatesTest (testList) where

import Data.Set hiding (singleton)
import Data.Map hiding (singleton, notMember, elems)
import MuCalc.States
import Test.HUnit hiding (State)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Function

testList = [ testProperty "empty bottom" emptyBottom
           , testProperty "full top" fullTop
           , testProperty "singleton" singletonProp
           , testProperty "from explicit set property" fromExplicitProperty
           , testProperty "explicit bijections" explicitBijectionProperty
           , testProperty "transition property" transitionProperty
           , testProperty "transition pullback property" transitionPullbackProperty
           , testCase "map to state list" mapToStateListTest
           ]

--Generators--

dimensions = elements [1..5]
dimNStates n = vectorOf n $ elements [True, False]

--If prop doesn't need to know about the dimension
forAllStates prop = forAll dimensions $ (\n ->
                    forAll (dimNStates n) $ prop)

--Generates transition functions which XOR a state with a list of bit masks
xorMaskGen = dimNStates

transitions :: Int -> Gen (State -> ExplicitStateSet)
transitions n = (\masks -> (\state ->
                    Explicit (Data.Set.map (\bit ->
                      Prelude.map (/=bit) state)
                    (Data.Set.fromList masks)) n)
                  ) `fmap` (xorMaskGen n)

instance Show (State -> ExplicitStateSet) where
  show f = "tough luck"

--Properties--

emptyBottom = forAll dimensions $ (\n ->
              forAll (dimNStates n) $ (not . (newBottom n `contains`)))

fullTop = forAll dimensions $ (\n ->
          forAll (dimNStates n) $ (newTop n `contains`))

singletonProp = forAllStates $ (\state -> (singleton state) `contains` state)

fromExplicitProperty = forAll dimensions $ (\n ->
                       forAll (listOf (dimNStates n)) $ (\states ->
                         let stateSet = fromExplicit (Explicit (Data.Set.fromList states) n)
                          in Prelude.all (stateSet `contains`) states))

explicitBijectionProperty = forAll dimensions $ (\n ->
                            forAll (listOf (dimNStates n)) $ (\list ->
                              let explicit = (Explicit (Data.Set.fromList list) n)
                               in explicit == toExplicit (fromExplicit explicit)))

--Expect the [output, input] pair of n-vector tuples
transitionProperty = forAll dimensions $ (\n ->
                     forAll (transitions n) $ (\f ->
                     forAll (dimNStates n) $ (\state ->
                       let transition = fromFunction n f
                           expected = Data.Set.map (++state) (states $ f state)
                        --Verify that the transition set contains every expected value.
                        in notMember False (Data.Set.map (transition `contains`) expected))))

--Check that the transition pullback function pulls back to states from which we can reach the goal image.
transitionPullbackProperty = forAll dimensions $ (\n ->
                             forAll (transitions n) $ (\f ->
                             forAll (listOf (dimNStates n)) $ (\stateList ->
                               let transition = fromFunction n f
                                   image = fromExplicit (Explicit (Data.Set.fromList stateList) n)
                                   preImage = elems.states.toExplicit $ throughTransition image transition
                                   hasAnyFResultsInImage = (\preImageState ->
                                                           Prelude.any (image `contains`) (elems.states.f $ preImageState))
                                in Prelude.all hasAnyFResultsInImage preImage)))

--TODO: rewrite as the property that the result should have 2^|free variables| elements.
mapToStateListTest = let hash = Data.Map.fromList [(4, True), (6, False)]
                         expected = [ [True, True, False, True]
                                    , [True, True, False, False]
                                    , [True, False, False, True]
                                    , [True, False, False, False] ]
                      in Data.Set.fromList (rebaseMapToState 4 hash) @?= Data.Set.fromList expected
