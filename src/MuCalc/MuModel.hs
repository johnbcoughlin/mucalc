module MuCalc.MuModel where

import OBDD
import Data.Map
import MuCalc.MuFormula
import MuCalc.States hiding (dimension)

data MuModel = MuModel { dimension :: Int --Number of atomic propositions, thus the length of the boolean state vectors.
                       , transitions :: (Map TransitionLabel Transition) --Set of actions represented as transition relations.
                       , bottom :: StateSet
                       , top :: StateSet
                       }

newMuModel :: Int -> MuModel
newMuModel n = MuModel { dimension=n, transitions=Data.Map.empty, bottom=(newBottom n), top=(newTop n) }

--Construct the set of states of this model which satisfy the given formula.
realize :: MuFormula -> MuModel -> StateSet
realize (Proposition n) = realizeProposition n
realize (Negation formula) = realizeNegation formula
realize (Or f1 f2) = realizeDisjunction f1 f2
realize (And f1 f2) = realizeConjunction f1 f2

realizeProposition :: Int -> MuModel -> StateSet
realizeProposition i model = setAnd (top model) (Implicit (unit i True) 1)

realizeNegation :: MuFormula -> MuModel -> StateSet
realizeNegation formula model = setNot (realize formula model)

realizeDisjunction :: MuFormula -> MuFormula -> MuModel -> StateSet
realizeDisjunction formula1 formula2 model = setOr (realize formula1 model) (realize formula2 model)

realizeConjunction :: MuFormula -> MuFormula -> MuModel -> StateSet
realizeConjunction formula1 formula2 model = setAnd (realize formula1 model) (realize formula2 model)
