module MuCalc.MuModel ( State
                      , MuModel
                      , newMuModel
                      , actions, props, bottom, top
                      , withProps
                      , withProp
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

-- | Type class for logical states which can be encoded as boolean vectors.
-- The contract is that the encoding is injective, but not surjective. it is
-- obviously onerous to guarantee that every logical state space has precisely
-- 2^n elements. Thus, given an arbitrary boolean n-vector, we don't need to
-- guarantee that it decodes to a meaningful state.
--
-- More abstractly, we should have decode (encode s) == Just s for all s,
-- but we might have decode p == Nothing for some boolean vector p.
class State s where
  -- | Map a logical state to a boolean vector
  encode :: s -> PState
  -- | Tries to decode a boolean vector to a logical state
  decode :: PState -> Maybe s
  

data MuModel s = MuModel { props :: (M.Map String (s -> Bool))
                         , actions :: (M.Map String (s -> [s]))
                         }
                         deriving (Show)

-- | Construct a new empty model with the given state type.
newMuModel :: State s => MuModel s
newMuModel = MuModel { props=M.empty
                     , actions=M.empty
                     }

-- | Add a collection of propositions with labels to the model.
withProps :: State s => MuModel s -> M.Map String (s -> Bool) -> MuModel s
withProps m propositionMap = let mProps = M.map encodeProp propositionMap
                              in m {props=mProps}

-- | Add a single labeled proposition to the model.
withProp :: State s => MuModel s -> String -> (s -> Bool) -> MuModel s
withProp m label prop = let pprop = encodeProp prop
                            mProps = M.insert label pprop (actions m)
                         in m {props=mProps}

encodeProp :: State s => (s -> Bool) -> PProp
encodeProp f = let ppred v = case decode v of
                              Just s -> f s
                              Nothing -> False
                in predicateToPProp ppred

-- | Add a collection of actions with labels to the model.
withActions :: State s => MuModel s -> M.Map String (s -> [s]) -> MuModel s
withActions m trs = let mTrs = M.map encodeAction trs
                     in m {actions=mTrs}

-- | Add a single labeled action to the model.
withAction :: State s => MuModel s -> String -> (s -> [s]) -> MuModel s
withAction m label tr = let physicalTr = encodeAction tr
                            mTrs = M.insert label physicalTr (actions m)
                         in m {actions=mTrs}

encodeAction :: State s => (s -> [s]) -> PAction
encodeAction f = let g = map encode . f -- :: s -> [PState]
                     h ps = case decode ps of
                                 Nothing -> []
                                 Just s -> g s
                    in fanoutToPAction h
