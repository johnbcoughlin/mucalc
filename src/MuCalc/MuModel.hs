module MuCalc.MuModel ( State
                      , MuModel
                      , newMuModel
                      , dimension, actions, props, bottom, top
                      , withPropositions
                      , withProposition
                      , withActions
                      , withAction
                      )
                      where

import Prelude hiding (lookup)
import OBDD hiding ((&&), not)
import qualified Data.Map as M
import MuCalc.States
import Control.Exception
import Data.Maybe

class State s where
  -- | Map a logical state to a boolean vector
  encode :: s -> PState
  -- | Decode a boolean vector to a logical state
  decode :: PState -> Maybe s

data (State s) => MuModel s = MuModel { dimension :: Int --Number of atomic propositions, thus the length of the boolean state vectors.
                                      , props :: (M.Map String PhysicalProposition)
                                      , actions :: (M.Map String PAction) --Set of actions represented as transition relations.
                                      , bottom :: [s]
                                      , top :: [s]
                                      }
                                      deriving (Show)

-- | Construct a new empty model with the given dimension.
newMuModel :: State s => Int -> MuModel s
newMuModel n = MuModel { dimension=n
                       , props=M.empty
                       , actions=M.empty
                       , bottom=[]
                       , top=[]
                       }

-- | Add a collection of propositions with labels to the model.
withPropositions :: State s => MuModel s -> M.Map String (s -> Bool) -> MuModel s
withPropositions m propositionMap = let n = dimension m
                                        mProps = M.map (preprocessProposition n) propositionMap
                                     in m {props=mProps}

-- | Add a single labeled proposition to the model.
withProposition :: State s => MuModel s -> String -> (s -> Bool) -> MuModel s
withProposition m label prop = let n = dimension m
                                   physicalProp = preprocessProposition n prop
                                   mProps = M.insert label physicalProp (actions m)
                                in m {props=mProps}

preprocessProposition :: State s => Int -> (s -> Bool) -> PhysicalProposition
preprocessProposition n f = predicateToPhysicalProposition n (f . decode)

-- | Add a collection of actions with labels to the model.
withActions :: State s => MuModel s -> M.Map String (s -> [s]) -> MuModel s
withActions m trs = let n = dimension m
                            mTrs = M.map (encodeAction n) trs
                         in m {actions=mTrs}

-- | Add a single labeled action to the model.
withAction :: State s => MuModel s -> String -> (s -> [s]) -> MuModel s
withAction m label tr = let n = dimension m
                                physicalTr = encodeAction n tr
                                mTrs = M.insert label physicalTr (actions m)
                             in m {actions=mTrs}

encodeAction :: State s => Int -> (s -> [s]) -> PAction
encodeAction n f = fanoutToPAction n (map encode . f . decode)
