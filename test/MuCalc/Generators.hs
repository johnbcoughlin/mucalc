{-# LANGUAGE FlexibleInstances #-}
module MuCalc.Generators ( dimensions
                         , dimNStates
                         , models
                         , pairsOf
                         , forAllModels
                         , forAllModelsSuchThat
                         , forAllStates
                         , formulas
                         , negatableFormulas
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
import MuCalc.MuFormula
import MuCalc.MuModel
import Test.QuickCheck
import Test.QuickCheck.Function

dimensions = elements [1..3]
dimNStates n = vectorOf n $ elements [True, False]

pairsOf :: Gen a -> Gen (a, a)
pairsOf g = pure (,) <*> g <*> g

--If prop doesn't need to know about the dimension
forAllStates prop = forAll dimensions (\n ->
                    forAll (dimNStates n) prop)

type ModelTriple = (MuModel, Gen PState)
forAllModels :: (ModelTriple -> Property) -> Property
forAllModels = forAllModelsSuchThat $ const True

forAllModelsSuchThat :: (MuModel -> Bool) -> (ModelTriple -> Property) -> Property
forAllModelsSuchThat test prop = forAll dimensions (\n ->
                                 forAll (models n `suchThat` test)  (\model ->
                                   let stateGen = dimNStates n
                                    in prop (model, stateGen)))


models :: Int -> Gen MuModel
models n = let base = (newMuModel n) `withProps` (eqPropositions n)
               actionLists = listOf (iffActions n)
            in (\aList -> let count = length aList
                              itoa i = "A:" ++ [chr (i + 65)]
                              aNames = map itoa [0..count]
                              aMap = M.fromList $ zip aNames aList
                           in base `withActions` aMap) <$> actionLists

--A map of named propositions, each of which checks the value of a state at the given index.
eqPropositions n = let forIndex i = (!!i)
                       singletons = map (\i -> M.singleton (show i) (forIndex i)) [0..n-1]
                    in M.unions singletons

--Generates action functions which IFF a state with a list of bit masks,
--plus the current state.
iffMaskGen = dimNStates

mapMasks :: Int -> [PState] -> PState -> [PState]
mapMasks n masks state = map (zipWith (&&) state) (state : masks)

iffActions :: Int -> Gen (PState -> [PState])
iffActions n = ((mapMasks n) `fmap` listOf (iffMaskGen n))

instance Show (PState -> [PState]) where
  show f = "tough luck"

--Generators for MuFormulas. Each generator uses a FormulaGenContext to govern dispatch.

baseContext :: MuModel -> FormulaGenContext
baseContext m = FormulaGenContext (dimension m) True (M.keys (props m)) (M.keys (actions m)) [] [] M.empty

data FormulaGenBranch = Prop | Var | Neg | Conj | Disj | Action | Fixp
                      deriving (Eq, Show, Ord)

data FormulaGenContext = FormulaGenContext { dim :: Int
                                           , prty :: Bool
                                           , pLabels :: [String]
                                           , aLabels :: [String]
                                           , vars :: [String]
                                           , usedVars :: [String]
                                           , freqs :: M.Map FormulaGenBranch Int
                                           }

cf c k = M.findWithDefault 0 k . freqs $ c

--We reduce branching every time it happens to ensure that the generator terminates in a timely fashion.
--"Branching" also includes linear operators like negation and action.
reduceBranching context = let dec i = if i == 0 then 0 else i - 1
                              newFreqs = foldr (M.adjust dec) (freqs context) [Disj, Conj, Neg, Fixp, Var, Action]
                           in (context {freqs = newFreqs})

allFormulas :: FormulaGenContext -> Gen MuFormula
allFormulas c = frequency [ (cf c Prop, atoms c)
                          , (cf c Var, if prty c
                                  then (if length (vars c) == 1
                                        then variables c
                                        else disjunctions c)
                                  else negations c)
                       , (cf c Neg, negations c)
                       , (cf c Disj, disjunctions c)
                       , (cf c Conj, conjunctions c)
                       , (cf c Action, if not (null (aLabels c))
                                    then possiblyNexts c
                                    else conjunctions c)
                       , (cf c Fixp, fixpointOperators c)
                       ]

formulas model = let cfs = M.fromList [(Prop, 2), (Var, 2), (Neg, 2), (Disj, 2),
                                       (Conj, 2), (Action, 2), (Fixp, 2)]
                     c = baseContext model
                  in allFormulas (c {freqs = cfs})

--No variables
negatableFormulas model = let cfs = M.fromList [(Prop, 3), (Disj, 2), (Conj, 2), (Action, 2)]
                              c = baseContext model
                           in allFormulas (c {freqs = cfs})

atoms c = Atom <$> elements (pLabels c)

negations c = let p = prty c
                  newContext = (reduceBranching c) {prty = not p}
               in pure Negation <*> allFormulas newContext

--If there are variables left to use, then assign one to the left and the rest to the right
disjunctions context = let v = vars context
                           c = reduceBranching context
                        in if null v
                           then pure Or <*> allFormulas c <*> allFormulas c
                           else pure Or <*> allFormulas (c { vars = [head v] })
                                        <*> allFormulas (c { vars = (tail v) })

conjunctions context = let v = vars context
                           c = reduceBranching context
                        in if null v
                           then pure And <*> allFormulas c  <*> allFormulas c
                           else pure And <*> allFormulas (c { vars = [head v] })
                                         <*> allFormulas (c { vars = (tail v) })

--Pick a variable at random from the list of bound vars.
--We'll check if the parity is right in the top level generator
variables c = pure Variable <*> elements (vars c)

--Pick an action label at random from the list of known actions
possiblyNexts c = pure PossiblyNext <*> elements (aLabels c) <*> allFormulas c

fixpointOperators c = let used = usedVars c
                          i = if null used
                              then 65 --start with "A"
                              else (ord . head . head $ used) + 1 --otherwise the first unused character
                          var = [chr i]
                          newUsed = var:used
                          newVars = var:(vars c)
                          newContext = (reduceBranching c) {vars=newVars, usedVars=newUsed}
                       in pure (Mu var) <*> allFormulas newContext

--Convenient properties--

subsetN :: Int -> StateSet -> StateSet -> Property
subsetN n p q = forAll (dimNStates n) (\state ->
                  p `contains` state ==> q `contains` state)
