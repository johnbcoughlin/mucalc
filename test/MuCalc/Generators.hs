{-# LANGUAGE FlexibleInstances #-}
module MuCalc.Generators where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Control.Applicative
import Data.Functor
import MuCalc.States
import MuCalc.MuFormula
import MuCalc.MuModel
import Test.QuickCheck
import Test.QuickCheck.Function

dimensions = elements [1..3]
dimNStates n = vectorOf n $ elements [True, False]

pairsOf :: Gen a -> Gen (a, a)
pairsOf g = pure (,) <*> g <*> g

--If prop doesn't need to know about the dimension
forAllStates prop = forAll dimensions $ (\n ->
                    forAll (dimNStates n) $ prop)

forAllModels :: ((MuModel, Gen MuFormula, Gen State) -> Property) -> Property
forAllModels prop = forAll dimensions $ (\n ->
                    forAll (models n) $ (\model ->
                      let formulaGen = baseFormulaGen model
                          stateGen = dimNStates n
                       in prop (model, formulaGen, stateGen)))


models :: Int -> Gen MuModel
models n = let base = newMuModel n
               transitionLists = listOf $ (fromFunction n <$> iffTransitions n) -- :: Gen [Transition]
            in (\trList -> let count = length trList
                               itoa = (\i -> "Tr:" ++ [chr (i + 65)])
                               trNames = map itoa [0..count]
                               trMap = M.fromList $ zip trNames trList
                            in base { transitions=trMap }) <$> transitionLists

--Generates transition functions which IFF a state with a list of bit masks,
--plus the current state.
iffMaskGen = dimNStates

mapMasks :: Int -> [State] -> State -> ExplicitStateSet
mapMasks n masks state = Explicit (S.fromList (map (zipWith (&&) state)
                                                   (state : masks))) n

iffTransitions :: Int -> Gen (State -> ExplicitStateSet)
iffTransitions n = (mapMasks n) `fmap` (listOf $ iffMaskGen n)

instance Show (State -> ExplicitStateSet) where
  show f = "tough luck"

baseFormulaGen :: MuModel -> Gen MuFormula
baseFormulaGen m = formulas (dimension m)
                            True
                            (M.keys (transitions m))
                            []

--                  dim    parity  transitions          variables in scope
type MuFormulaGen = Int -> Bool -> [TransitionLabel] -> [String] -> Gen MuFormula

formulas :: MuFormulaGen
formulas n parity trs vars = let fn = formulas n parity trs vars
                              in frequency [ (4, propositions n)
                                           , (1, pure Negation <*> fn)
                                           , (1, pure Or <*> fn <*> fn)
                                           , (1, pure And <*> fn <*> fn)
                                           ]

propositions n = Proposition <$> elements [0..n-1]

negations :: MuFormulaGen
negations n parity trs vars = pure Negation <*> formulas n (not parity) trs vars

disjunctions :: MuFormulaGen
disjunctions n parity trs vars = let fs = formulas n parity trs vars
                                  in pure Or <*> fs <*> fs

conjunctions :: MuFormulaGen
conjunctions n parity trs vars = let fs = formulas n parity trs vars
                                  in pure And <*> fs <*> fs

--Pick a variable at random from the list of bound vars.
--We'll check if the parity is right in the top level generator
variables :: MuFormulaGen
variables n parity trs vars = pure Variable <*> elements vars

--Pick a transition label at random from the list of known transitions
possiblyNexts :: MuFormulaGen
possiblyNexts n parity trs vars = let fs = formulas n parity trs vars
                                   in pure PossiblyNext <*> elements trs <*> fs

fixpointOperators :: MuFormulaGen
fixpointOperators n parity trs vars = let i = if null vars
                                              then 65 --start with "A"
                                              else (ord . head . head $ vars) + 1 --otherwise the first unused character
                                          var = [chr i]
                                          newVars = var:vars
                                       in pure (Mu var) <*> formulas n parity trs newVars

subsetN :: Int -> StateSet -> StateSet -> Property
subsetN n p q = forAll (dimNStates n) $ (\state ->
                  p `contains` state ==> q `contains` state)
