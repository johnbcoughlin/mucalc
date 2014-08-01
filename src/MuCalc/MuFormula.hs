module MuCalc.MuFormula where

import Data.Map
import MuCalc.MuModel

data MuFormula = Atom String | --Atomic proposition
                 Variable String | --Unbound variable
                 Negation MuFormula | Or MuFormula MuFormula | And MuFormula MuFormula |
                 PossiblyNext String MuFormula | --(E X): If there's an action with that label leading to a state where the formula holds.
                 Mu String MuFormula --Least fixpoint
                deriving (Show)

implies :: MuFormula -> MuFormula -> MuFormula
implies p q = Or (Negation p) q

iff :: MuFormula -> MuFormula -> MuFormula
iff p q = And (implies p q) (implies q p)
