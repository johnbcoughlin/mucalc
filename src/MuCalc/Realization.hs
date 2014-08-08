module MuCalc.Realization  where

import Prelude hiding (lookup)
import OBDD hiding ((&&), not)
import qualified Data.Map as M
import MuCalc.MuFormula
import GRSynth.Semantics
import MuCalc.States
import Control.Exception

data RealizationError = VariableParityError | UnknownPropositionError |
                        InvalidActionError | AlreadyBoundVariableError |
                        UnknownVariableError
    deriving (Eq, Show)

type Realization s = Either RealizationError [s]

type PhysicalRealization = Either RealizationError StateSet

-- | Context for the evaluation of a model realization. Includes atomic propositions
-- encoded as StateSets, and actions encoded as transition relations, i.e. as StateSets.
data Context = Context { parity :: Bool
                       , env :: M.Map String StateSet
                       , atoms :: M.Map String PProp
                       , transitions :: M.Map String PAction
                       }

--Construct the set of states of this model which satisfy the given formula.
realizeTop :: State s => MuFormula -> Realization s
realizeTop phi = case realize phi Context {parity=True, env=M.empty} of
                   Left e -> Left e
                   Right stateSet -> Right (map strictDecode $ toExplicit stateSet)

strictDecode :: State s => PState -> s
strictDecode pState = case decode pState of
                           Nothing -> error ("Undecodeable physical state: " ++ show pState)
                           Just (s, []) -> s
                           Just (s, _) -> error ("Lossy decoding: " ++ show pState)

realize :: MuFormula -> Context -> PhysicalRealization
realize (Atom p) = realizeAtom p
realize (Negation f) = realizeNegation f
realize (Or f1 f2) = realizeDisjunction f1 f2
realize (And f1 f2) = realizeConjunction f1 f2
realize (Variable var) = realizeVariable var
realize (PossiblyNext action f) = realizeAction action f
realize (Mu var f) = realizeMu var f

realizeAtom :: String -> Context -> PhysicalRealization
realizeAtom p c = case M.lookup p (atoms c) of
                      Nothing -> Left UnknownPropositionError
                      Just stateSet -> Right stateSet

realizeNegation :: MuFormula -> Context -> PhysicalRealization
realizeNegation f c = let newContext = c {parity = not (parity c)}
                          r = realize f newContext
                       in case r of
                               Left e -> Left e
                               Right set -> Right $ setNot set

realizeDisjunction :: MuFormula -> MuFormula -> Context -> PhysicalRealization
realizeDisjunction f1 f2 c = let r1 = realize f1 c
                                 r2 = realize f2 c
                              in combine setOr r1 r2

realizeConjunction :: MuFormula -> MuFormula -> Context -> PhysicalRealization
realizeConjunction f1 f2 c = let r1 = realize f1 c
                                 r2 = realize f2 c
                              in combine setAnd r1 r2

realizeVariable :: String -> Context -> PhysicalRealization
realizeVariable var c = case M.lookup var (env c) of
                             Nothing -> Left UnknownVariableError
                             Just set -> if parity c
                                         then Right set
                                         else Left VariableParityError

realizeAction :: String -> MuFormula -> Context -> PhysicalRealization
realizeAction label f c = case M.lookup label (transitions c) of
                               Nothing -> Left InvalidActionError
                               Just action -> let phiRealization = realize f c
                                               in case phiRealization of
                                                       Left error -> Left error
                                                       Right states -> Right (states `throughAction` action)

realizeMu :: String -> MuFormula -> Context -> PhysicalRealization
realizeMu var f c = case M.lookup var (env c) of
                         Just _ -> Left AlreadyBoundVariableError
                         Nothing -> leastFixpoint var f c

leastFixpoint :: String -> MuFormula -> Context -> PhysicalRealization
leastFixpoint var f c = let loop = fixpointLoop var f c
                            test = (==)
                         in fixpoint loop test (Right newBottom)

fixpointLoop :: String -> MuFormula -> Context -> PhysicalRealization -> PhysicalRealization
fixpointLoop var f c (Left error) = Left error
fixpointLoop var f c (Right state) = let newEnv = M.insert var state (env c)
                                      in realize f (c {env=newEnv})

--Should be TCOed
fixpoint :: (a -> a) -> (a -> a -> Bool) -> a -> a
fixpoint f eq init = let next = f init
                      in if next `eq` init
                         then next
                         else fixpoint f eq next

combine :: (StateSet -> StateSet -> StateSet) -> PhysicalRealization -> PhysicalRealization -> PhysicalRealization
combine f r1 r2 = case r1 of
                    Left e -> Left e
                    Right states1 -> (case r2 of
                      Left e -> Left e
                      Right states2 -> Right (f states1 states2))

