module MuCalc.MuFormula where

import Data.Map

type TransitionLabel = String

data MuFormula = Proposition Int | --Atomic proposition
                 Variable String | --Unbound variable
                 Negation MuFormula | Or MuFormula MuFormula | And MuFormula MuFormula
                 --PossiblyNext TransitionLabel MuFormula | --(E X): If there's a transition with that label leading to a state where the formula holds.
                 --Mu String MuFormula --Least fixpoint
                deriving (Show)
