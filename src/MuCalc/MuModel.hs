module MuCalc.MuModel where

import OBDD hiding ((&&))
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

data RealizationError = VariableParityError | PropositionIndexError

type Realization = Either RealizationError StateSet

--Construct the set of states of this model which satisfy the given formula.
realize :: MuFormula -> MuModel -> StateSet
realize phi model = case realizeAux phi empty model of
                      Left error -> newBottom (dimension model)
                      Right result -> result

realizeAux :: MuFormula -> Context -> MuModel -> Realization
realizeAux (Proposition n) = realizeProposition n
realizeAux (Negation f) = realizeNegation f
realizeAux (Or f1 f2) = realizeDisjunction f1 f2
realizeAux (And f1 f2) = realizeConjunction f1 f2

realizeProposition :: Int -> Context -> MuModel -> Realization
realizeProposition i _ m = case (0 <= i) && (i < dimension m) of
                             False -> Left PropositionIndexError
                             True -> Right $ (top m) `setAnd` (Implicit (unit i True) 1)

realizeNegation :: MuFormula -> Context -> MuModel -> Realization
realizeNegation f c m = let r = realizeAux f c m
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

combine :: (StateSet -> StateSet -> StateSet) -> Realization -> Realization -> Realization
combine f r1 r2 = case r1 of
                    Left e -> Left e
                    Right states1 -> (case r2 of
                      Left e -> Left e
                      Right states2 -> Right (f states1 states2))

