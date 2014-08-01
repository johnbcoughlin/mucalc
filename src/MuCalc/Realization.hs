module MuCalc.Realization  where

import Prelude hiding (lookup)
import OBDD hiding ((&&), not)
import qualified Data.Map as M
import MuCalc.MuFormula
import MuCalc.MuModel
import MuCalc.States hiding (dimension)
import Control.Exception

data RealizationError = VariableParityError | PropositionIndexError |
                        InvalidTransitionError | AlreadyBoundVariableError |
                        UnknownVariableError
    deriving (Eq, Show)

type Realization = Either RealizationError StateSet

--Construct the set of states of this model which satisfy the given formula.
realize :: MuFormula -> MuModel -> Realization
realize phi = realizeAux phi (Context {parity=True, env=M.empty})

realizeAux :: MuFormula -> Context -> MuModel -> Realization
realizeAux (Proposition n) = realizeProposition n
realizeAux (Negation f) = realizeNegation f
realizeAux (Or f1 f2) = realizeDisjunction f1 f2
realizeAux (And f1 f2) = realizeConjunction f1 f2
realizeAux (Variable var) = realizeVariable var
realizeAux (PossiblyNext transition f) = realizeTransition transition f
realizeAux (Mu var f) = realizeMu var f

realizeProposition :: Int -> Context -> MuModel -> Realization
realizeProposition i _ m = case (0 <= i) && (i < dimension m) of
                             False -> Left PropositionIndexError
                             True -> Right $ (top m) `setAnd` (Implicit (unit i True) 1)

realizeNegation :: MuFormula -> Context -> MuModel -> Realization
realizeNegation f c m = let newContext = c { parity = not (parity c) }
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
realizeVariable var c m = case M.lookup var (env c) of
                            Nothing -> Left UnknownVariableError
                            Just set -> if parity c
                                        then Right set
                                        else Left VariableParityError

realizeTransition :: TransitionLabel -> MuFormula -> Context -> MuModel -> Realization
realizeTransition label f c m = case M.lookup label (transitions m) of
                                  Nothing -> Left InvalidTransitionError
                                  Just transition -> let phiRealization = realizeAux f c m
                                                      in case phiRealization of
                                                          Left error -> Left error
                                                          Right states -> Right (states `throughTransition` transition)

realizeMu :: String -> MuFormula -> Context -> MuModel -> Realization
realizeMu var f c m = case M.lookup var (env c) of
                        Just _ -> Left AlreadyBoundVariableError
                        Nothing -> leastFixpoint var f c m

leastFixpoint :: String -> MuFormula -> Context -> MuModel -> Realization
leastFixpoint var f c m = let loop = fixpointLoop var f c m
                              test = (==)
                           in fixpoint loop test (Right $ bottom m)

fixpointLoop :: String -> MuFormula -> Context -> MuModel -> Realization -> Realization
fixpointLoop var f c m (Left error) = Left error
fixpointLoop var f c m (Right state) = let newEnv = M.insert var state (env c)
                                        in realizeAux f (c {env=newEnv}) m

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

