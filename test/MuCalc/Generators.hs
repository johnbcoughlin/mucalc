{-# LANGUAGE FlexibleInstances #-}
module MuCalc.Generators where

import Data.Set hiding (singleton)
import Control.Applicative
import Data.Functor
import MuCalc.States
import MuCalc.MuFormula
import MuCalc.MuModel
import Test.QuickCheck
import Test.QuickCheck.Function

dimensions = elements [1..3]
dimNStates n = vectorOf n $ elements [True, False]

--If prop doesn't need to know about the dimension
forAllStates prop = forAll dimensions $ (\n ->
                    forAll (dimNStates n) $ prop)

--Generates transition functions which XOR a state with a list of bit masks
xorMaskGen = dimNStates

xorTransitions :: Int -> Gen (State -> ExplicitStateSet)
xorTransitions n = (\masks -> (\state ->
                    Explicit (Data.Set.map (\bit ->
                      Prelude.map (/=bit) state)
                    (Data.Set.fromList masks)) n)
                  ) `fmap` (xorMaskGen n)

instance Show (State -> ExplicitStateSet) where
  show f = "tough luck"

propositions n = Proposition <$> elements [0..n-1]

formulas :: Int -> Gen MuFormula
formulas n = let fn = formulas n
              in frequency [ (4, propositions n)
                           , (1, pure Negation <*> fn)
                           , (1, pure Or <*> fn <*> fn)
                           , (1, pure And <*> fn <*> fn)
                           ]

models :: Int -> Gen MuModel
models = pure . newMuModel
