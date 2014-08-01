module MuCalc.MuModel ( MuModel
                      , newMuModel
                      , dimension, transitions, props, bottom, top
                      , PropositionLabel
                      , Proposition (Proposition)
                      , withPropositions
                      , TransitionLabel
                      , Transition (Transition)
                      , withTransitions
                      )
                      where

import Prelude hiding (lookup)
import OBDD hiding ((&&), not)
import qualified Data.Map as M
import MuCalc.States
import Control.Exception

data MuModel = MuModel { dimension :: Int --Number of atomic propositions, thus the length of the boolean state vectors.
                       , transitions :: (M.Map TransitionLabel PhysicalTransition) --Set of actions represented as transition relations.
                       , props :: (M.Map PropositionLabel StateSet)
                       , bottom :: StateSet
                       , top :: StateSet
                       }
                       deriving (Show)

-- | Construct a new empty model with the given dimension.
newMuModel :: Int -> MuModel
newMuModel n = MuModel { dimension=n, transitions=M.empty, props=M.empty, bottom=(newBottom n), top=(newTop n) }

type PropositionLabel = String
-- | An atomic proposition over the model.
data Proposition = Proposition (State -> Bool)

-- | Add a collection of propositions with labels to the model.
withPropositions :: MuModel -> M.Map PropositionLabel Proposition -> MuModel
withPropositions m propositionMap = let n = dimension m
                                        mProps = M.map (preprocessProposition n) propositionMap
                                     in m {props=mProps}

preprocessProposition n (Proposition f) = predicateToPhysicalTransition n f

type TransitionLabel = String
-- | A non-deterministic transition function: maps states to possible successor states.
data Transition = Transition (State -> [State])

-- | Add a collection of transitions with labels to the model.
withTransitions :: MuModel -> M.Map TransitionLabel Transition -> MuModel
withTransitions m trs = let n = dimension m
                            mTrs = M.map (preprocessTransition n) trs
                         in m {transitions=mTrs}

preprocessTransition n (Transition f) = fanoutToPhysicalTransition n f
