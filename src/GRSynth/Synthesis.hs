module GRSynth.Synthesis ( synth
                         , cox
                         , fixpointStop
                         ) where

import GRSynth.Formulas
import GRSynth.States
import GRSynth.GameStructure
import GRSynth.Semantics
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List (foldl')

cox :: PAction -> PAction -> StateSet -> StateSet
cox env sys set = let poised = set `throughAction` sys --poised to move to a good position
                   in poised `forcedByAction` env

coxGS :: GameStructure -> StateSet -> StateSet
coxGS gs = cox (env gs) (sys gs)

type S = (Int, Int, Int)
tickJ, tickR, tickI :: SynthM ()
tickJ = let f (j, r, i) = (j+1, r, i)
         in modify f
tickR = let f (j, r, i) = (j, r+1, i)
         in modify f
tickI = let f (j, r, i) = (j, r, i+1)
         in modify f

type SynthM = ReaderT GameStructure (StateT S Identity)

synth :: GameStructure -> StateSet
synth gs = fst $ runSynth doSynth gs

runSynth :: SynthM StateSet -> GameStructure -> (StateSet, S)
runSynth comp gs = runIdentity (runStateT (runReaderT comp gs) (0, 0, 0))

doSynth :: SynthM StateSet
doSynth = doGFixpointZ newTop

doGFixpointZ :: StateSet -> SynthM StateSet
doGFixpointZ z = do gs <- ask
                    let n = length $ guarantees gs
                     in do newZ <- doGFixpointZAux n z
                           if fixpointStop newZ z
                              then return newZ
                              else put (0, 0, 0) >> doGFixpointZ newZ

--Execute the Y least fixpoint n times, binding the result to Z in every iteration
doGFixpointZAux :: Int -> StateSet -> SynthM StateSet
doGFixpointZAux n currentZ = do (j, _, _) <- get
                                if j == n
                                   then return currentZ
                                   else do nextZ <- doLFixpointY currentZ newBottom
                                           tickJ
                                           doGFixpointZAux n nextZ

doLFixpointY :: StateSet -> StateSet -> SynthM StateSet
doLFixpointY z y = do gs <- ask
                      (j, r, _) <- get
                      let guarantee = guarantees gs !! j
                          Right goodStates = realize (props gs) guarantee
                          coxZ = coxGS gs z
                          coxY = coxGS gs y
                          start = (goodStates `setAnd` coxZ) `setOr` coxY
                          m = length (assumptions gs)
                       in do disjuncts <- forM [0..m-1] (\i -> do
                                               x <- doGFixpointX start z
                                               put (j, r, i)
                                               return x)
                             let newY = foldl' setOr newBottom disjuncts
                              in if fixpointStop y newY
                                    then return newY
                                    else doLFixpointY z newY

doGFixpointX :: StateSet -> StateSet -> SynthM StateSet
doGFixpointX start x = do (_, _, i) <- get
                          gs <- ask
                          let assumption = assumptions gs !! i
                              Right badStates = realize (props gs) (Negation assumption)
                              coxX = coxGS gs x
                              newX = start `setOr` (badStates `setAnd` coxX)
                           in if fixpointStop newX x
                                 then return newX
                                 else doGFixpointX start newX

--Equality check on statesets, assuming phi <= psi
fixpointStop :: StateSet -> StateSet -> Bool
fixpointStop phi psi = empty (psi `setAnd` setNot phi)
