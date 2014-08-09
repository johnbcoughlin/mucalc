{-# LANGUAGE FlexibleInstances #-}
module MuCalc.Generators ( dimensions
                         , dimNStates
                         , pairsOf
                         , forAllStates
                         , iffActions
                         , subsetN
                         )
  where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Control.Applicative
import Data.Functor
import MuCalc.States
import Test.QuickCheck
import Test.QuickCheck.Function

dimensions = elements [1..3]
dimNStates n = vectorOf n $ elements [True, False]

pairsOf :: Gen a -> Gen (a, a)
pairsOf g = pure (,) <*> g <*> g

--If prop doesn't need to know about the dimension
forAllStates prop = forAll dimensions (\n ->
                    forAll (dimNStates n) prop)

--Generates action functions which IFF a state with a list of bit masks,
--plus the current state.
iffMaskGen = dimNStates

mapMasks :: Int -> [PState] -> PState -> [PState]
mapMasks n masks state = map (zipWith (&&) state) (state : masks)

iffActions :: Int -> Gen (PState -> [PState])
iffActions n = ((mapMasks n) `fmap` listOf (iffMaskGen n))

instance Show (PState -> [PState]) where
  show = const "tough luck"

--Convenient properties--

subsetN :: Int -> StateSet -> StateSet -> Property
subsetN n p q = forAll (dimNStates n) (\state ->
                  p `contains` state ==> q `contains` state)
