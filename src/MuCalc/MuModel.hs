module MuCalc.MuModel where

import Prelude hiding (lookup)
import OBDD hiding ((&&), not)
import qualified Data.Map as M
import MuCalc.MuFormula
import MuCalc.States hiding (dimension)
import Control.Exception

data MuModel = MuModel { dimension :: Int --Number of atomic propositions, thus the length of the boolean state vectors.
                       , transitions :: (M.Map TransitionLabel Transition) --Set of actions represented as transition relations.
                       , bottom :: StateSet
                       , top :: StateSet
                       }
                       deriving (Show)

newMuModel :: Int -> MuModel
newMuModel n = MuModel { dimension=n, transitions=M.empty, bottom=(newBottom n), top=(newTop n) }

data Context = Context { parity :: Bool
                       , env :: (M.Map String StateSet)
                       }
