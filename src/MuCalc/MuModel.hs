module MuCalc.MuModel where

import Prelude hiding (lookup)
import OBDD hiding ((&&), not)
import Data.Map
import MuCalc.MuFormula
import MuCalc.States hiding (dimension)
import Control.Exception

data MuModel = MuModel { dimension :: Int --Number of atomic propositions, thus the length of the boolean state vectors.
                       , transitions :: (Map TransitionLabel Transition) --Set of actions represented as transition relations.
                       , bottom :: StateSet
                       , top :: StateSet
                       }
                       deriving (Show)

newMuModel :: Int -> MuModel
newMuModel n = MuModel { dimension=n, transitions=Data.Map.empty, bottom=(newBottom n), top=(newTop n) }

type Context = Map String (StateSet, Bool)

data RealizationError = VariableParityError | PropositionIndexError |
                        InvalidTransitionError | VariableBindingError
    deriving (Eq)

type Realization = Either RealizationError StateSet

--Construct the set of states of this model which satisfy the given formula.
realize :: MuFormula -> MuModel -> Realization
realize phi = realizeAux phi empty

realizeAux :: MuFormula -> Context -> MuModel -> Realization
realizeAux (Proposition n) = realizeProposition n
realizeAux (Negation f) = realizeNegation f
realizeAux (Or f1 f2) = realizeDisjunction f1 f2
realizeAux (And f1 f2) = realizeConjunction f1 f2
realizeAux (Variable var) = realizeVariable var

realizeProposition :: Int -> Context -> MuModel -> Realization
realizeProposition i _ m = case (0 <= i) && (i < dimension m) of
                             False -> Left PropositionIndexError
                             True -> Right $ (top m) `setAnd` (Implicit (unit i True) 1)

realizeNegation :: MuFormula -> Context -> MuModel -> Realization
realizeNegation f c m = let newContext = Data.Map.map (\p -> (fst p, not (snd p))) c
                            r = realizeAux f newContext m
                         in case r of
                           Left e -> Left e
                           Right set -> Right $ setNot set

realizeDisjunction :: MuFormula -> MuFormula -> Context -> MuModel -> Realization
realizeDisjunction f1 f2 c m = let r1 = realizeAux f1 c m
                                   r2 = realizeAux f2 c m
                                in combine setOr r1 r2

realizeConjunction :: MuFormula -> MuFormula -> Context -> MuModel -> Realization
realizeConjunction f1 f2 c m = let r1 = realizeAux f1 c m
                                   r2 = realizeAux f2 c m
                                in combine setAnd r1 r2

realizeVariable :: String -> Context -> MuModel -> Realization
realizeVariable var c m = case lookup var c of
                            Nothing -> Left VariableBindingError
                            Just (set, parity) -> if parity
                                                  then Right set
                                                  else Left VariableParityError

realizeTransition :: TransitionLabel -> MuFormula -> Context -> MuModel -> Realization
realizeTransition label f c m = case lookup label (transitions m) of
                                  Nothing -> Left InvalidTransitionError
                                  Just transition -> let phiRealization = realizeAux f c m
                                                      in case phiRealization of
                                                          Left error -> Left error
                                                          Right states -> Right (states `throughTransition` transition)

realizeMu :: String -> MuFormula -> Context -> MuModel -> Realization
realizeMu var f c m = case lookup var c of
                        Just _ -> Left VariableBindingError
                        Nothing -> leastFixpoint var f c m

leastFixpoint :: String -> MuFormula -> Context -> MuModel -> Realization
leastFixpoint var f c m = let loop = fixpointLoop var f c m
                              test = (==)
                           in fixpoint loop test (Right $ bottom m)

fixpointLoop :: String -> MuFormula -> Context -> MuModel -> Realization -> Realization
fixpointLoop var f c m (Left error) = Left error
fixpointLoop var f c m (Right state) = let newContext = adjust (\p -> (state, snd p)) var c
                                        in realizeAux f newContext m

--Should be TCOed
fixpoint :: (a -> a) -> (a -> a -> Bool) -> a -> a
fixpoint f eq init = let next = f init
                      in if next `eq` init
                         then next
                         else fixpoint f eq next

combine :: (StateSet -> StateSet -> StateSet) -> Realization -> Realization -> Realization
combine f r1 r2 = case r1 of
                    Left e -> Left e
                    Right states1 -> (case r2 of
                      Left e -> Left e
                      Right states2 -> Right (f states1 states2))

