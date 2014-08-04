module MuCalc.Realization  where

import Prelude hiding (lookup)
import OBDD hiding ((&&), not)
import qualified Data.Map as M
import MuCalc.MuFormula
import MuCalc.MuModel
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
                       , env :: (M.Map String StateSet)
                       }

--Construct the set of states of this model which satisfy the given formula.
realize :: State s => MuFormula -> MuModel s -> Realization s
realize phi model = case realizeAux phi (Context {parity=True, env=M.empty}) model of
                         Left e -> Left e
                         Right stateSet -> error "foo"

realizeAux :: MuFormula -> Context -> MuModel s -> PhysicalRealization
realizeAux (Atom p) = realizeAtom p
realizeAux (Negation f) = realizeNegation f
realizeAux (Or f1 f2) = realizeDisjunction f1 f2
realizeAux (And f1 f2) = realizeConjunction f1 f2
realizeAux (Variable var) = realizeVariable var
realizeAux (PossiblyNext action f) = realizeAction action f
realizeAux (Mu var f) = realizeMu var f

realizeAtom :: String -> Context -> MuModel s -> PhysicalRealization
realizeAtom p _ m = case M.lookup p (props m) of
                      Nothing -> Left UnknownPropositionError
                      Just (stateSet) -> Right stateSet

realizeNegation :: MuFormula -> Context -> MuModel s -> PhysicalRealization
realizeNegation f c m = let newContext = c { parity = not (parity c) }
                            r = realizeAux f newContext m
                         in case r of
                           Left e -> Left e
                           Right set -> Right $ setNot set

realizeDisjunction :: MuFormula -> MuFormula -> Context -> MuModel s -> PhysicalRealization
realizeDisjunction f1 f2 c m = let r1 = realizeAux f1 c m
                                   r2 = realizeAux f2 c m
                                in combine setOr r1 r2

realizeConjunction :: MuFormula -> MuFormula -> Context -> MuModel s -> PhysicalRealization
realizeConjunction f1 f2 c m = let r1 = realizeAux f1 c m
                                   r2 = realizeAux f2 c m
                                in combine setAnd r1 r2

realizeVariable :: String -> Context -> MuModel s -> PhysicalRealization
realizeVariable var c m = case M.lookup var (env c) of
                            Nothing -> Left UnknownVariableError
                            Just set -> if parity c
                                        then Right set
                                        else Left VariableParityError

realizeAction :: String -> MuFormula -> Context -> MuModel s -> PhysicalRealization
realizeAction label f c m = case M.lookup label (actions m) of
                                  Nothing -> Left InvalidActionError
                                  Just action -> let phiRealization = realizeAux f c m
                                                      in case phiRealization of
                                                          Left error -> Left error
                                                          Right states -> Right (states `throughAction` action)

realizeMu :: String -> MuFormula -> Context -> MuModel s -> PhysicalRealization
realizeMu var f c m = case M.lookup var (env c) of
                        Just _ -> Left AlreadyBoundVariableError
                        Nothing -> leastFixpoint var f c m

leastFixpoint :: String -> MuFormula -> Context -> MuModel s -> PhysicalRealization
leastFixpoint var f c m = let loop = fixpointLoop var f c m
                              test = (==)
                           in fixpoint loop test (Right newBottom)

fixpointLoop :: String -> MuFormula -> Context -> MuModel s -> PhysicalRealization -> PhysicalRealization
fixpointLoop var f c m (Left error) = Left error
fixpointLoop var f c m (Right state) = let newEnv = M.insert var state (env c)
                                        in realizeAux f (c {env=newEnv}) m

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

