module GRSynth.Formulas where

import qualified MuCalc.MuFormula as M

data SimpleFormula = Atom String | Negation SimpleFormula |
                     Or SimpleFormula SimpleFormula | And SimpleFormula SimpleFormula

convert :: SimpleFormula -> M.MuFormula
convert (Atom s) = M.Atom s
convert (Negation f) = M.Negation (convert f)
convert (Or f1 f2) = M.Or (convert f1) (convert f2)
convert (And f1 f2) = M.And (convert f1) (convert f2)
