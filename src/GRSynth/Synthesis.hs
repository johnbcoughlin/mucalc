module GRSynth.Synthesis where

import GRSynth.Formulas
import GRSynth.States
import GRSynth.GameStructure
import GRSynth.Semantics
import Control.Monad.Identity
import Data.List (foldl')

cox :: PAction -> PAction -> StateSet -> StateSet
cox env sys set = let poised = set `throughAction` sys --poised to move to a good position
                   in poised `forcedByAction` env

type SynthM = Identity StateSet

synth :: (State x, State y) => GameStructure x y -> StateSet
synth gs = runSynth (doSynth gs)

runSynth :: SynthM -> StateSet
runSynth = runIdentity

doSynth :: (State x, State y) => GameStructure x y -> SynthM
doSynth gs = doGFixpointZ gs newTop

doGFixpointZ :: (State x, State y) => GameStructure x y -> StateSet -> SynthM
doGFixpointZ gs z = do n <- return $ length (guarantees gs)
                       newZ <- doGFixpointZAux gs 0 n z
                       if fixpointStop newZ z
                          then return newZ
                          else doGFixpointZ gs newZ

--Execute the Y least fixpoint n times, binding the result to Z in every iteration
doGFixpointZAux :: (State x, State y) => GameStructure x y -> Int -> Int -> StateSet -> SynthM
doGFixpointZAux gs j n currentZ = if j == n
                                     then return currentZ
                                     else do
                                       nextZ <- doLFixpointY gs currentZ newBottom j
                                       doGFixpointZAux gs (j+1) n nextZ

doLFixpointY :: (State x, State y) => GameStructure x y -> StateSet -> StateSet -> Int -> SynthM
doLFixpointY gs z y j = do guarantee <- return $ guarantees gs !! j
                           Right goodStates <- return $ realize (props gs) guarantee
                           coxZ <- return $ cox (env gs) (sys gs) z
                           coxY <- return $ cox (env gs) (sys gs) y
                           start <- return $ (goodStates `setAnd` coxZ) `setOr` coxY
                           m <- return $ length (assumptions gs)
                           disjuncts <- forM [0..m-1] (\i -> do
                                x <- doGFixpointX gs start z i
                                return x)
                           newY <- return $ foldl' setOr newBottom disjuncts
                           if fixpointStop y newY
                              then return newY
                              else doLFixpointY gs z newY j

doGFixpointX :: (State x, State y) => GameStructure x y -> StateSet -> StateSet -> Int -> SynthM
doGFixpointX gs start x i = do assumption <- return $ assumptions gs !! i
                               Right badStates <- return $ realize (props gs) (Negation assumption)
                               coxX <- return $ cox (env gs) (sys gs) x
                               newX <- return $ start `setOr` (badStates `setAnd` coxX)
                               if fixpointStop newX x
                                  then return newX
                                  else doGFixpointX gs start newX i

--Equality check on statesets, assuming phi <= psi
fixpointStop :: StateSet -> StateSet -> Bool
fixpointStop phi psi = empty (psi `setAnd` (setNot phi))
