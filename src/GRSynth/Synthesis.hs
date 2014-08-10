module GRSynth.Synthesis where

import GRSynth.Formulas
import GRSynth.States
import GRSynth.GameStructure
import Control.Monad.Identity

cox :: PAction -> PAction -> StateSet -> StateSet
cox env sys set = let poised = set `throughAction` sys --poised to move to a good position
                   in poised `forcedByAction` env
