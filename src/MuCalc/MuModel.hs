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
                       deriving (Show)

newMuModel :: Int -> MuModel
newMuModel n = MuModel { dimension=n, transitions=Data.Map.empty, bottom=(newBottom n), top=(newTop n) }

type Context = Map String (StateSet, Bool)

--Construct the set of states of this model which satisfy the given formula.
realize :: MuFormula -> MuModel -> StateSet
realize phi model = realizeAux phi empty model

realizeAux :: MuFormula -> Context -> MuModel -> StateSet
realizeAux (Proposition n) = realizeProposition n
realizeAux (Negation f) = realizeNegation f
realizeAux (Or f1 f2) = realizeDisjunction f1 f2
realizeAux (And f1 f2) = realizeConjunction f1 f2

realizeProposition :: Int -> Context -> MuModel -> StateSet
realizeProposition i _ m = (top m) `setAnd` (Implicit (unit i True) 1)

realizeNegation :: MuFormula -> Context -> MuModel -> StateSet
realizeNegation f c m = setNot (realizeAux f c m)

realizeDisjunction :: MuFormula -> MuFormula -> Context -> MuModel -> StateSet
realizeDisjunction f1 f2 c m = (realizeAux f1 c m) `setOr` (realizeAux f2 c m)

realizeConjunction :: MuFormula -> MuFormula -> Context -> MuModel -> StateSet
realizeConjunction f1 f2 c m = (realizeAux f1 c m) `setAnd` (realizeAux f2 c m)
