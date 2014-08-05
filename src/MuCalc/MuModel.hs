module MuCalc.MuModel ( State , encode, decode, domain
                      , MuModel
                      , newMuModel
                      , actions, props
                      , withProp
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

data MuModel s = MuModel { props :: (M.Map String PProp)
                         , actions :: (M.Map String PAction)
                         -- | The entire state space. We do our best not to iterate over this,
                         -- but when for example a proposition is encoded as a predicate on states,
                         -- it's unavoidable.
                         , domain :: [s]
                         }

-- | Construct a new empty model with the given state type.
newMuModel :: State s => MuModel s
newMuModel = MuModel { props=M.empty
                     , actions=M.empty
                     }

-- | Add a single labeled proposition to the model.
withProp :: State s => MuModel s -> String -> (s -> Bool) -> MuModel s
withProp m label prop = let f pState = maybe (error "Couldn't decode PState") prop (decode pState)
                            pStates = map encode (domain m)
                            mProps = M.insert label (fromPredicate pStates f) (props m)
                         in m {props=mProps}

-- | Add a single labeled action to the model.
withAction :: State s => MuModel s -> String -> (s -> [s]) -> MuModel s
withAction m label tr = let f pState = map encode (maybe (error "Couldn't decode PState") tr (decode pState))
                            pStates = map encode (domain m)
                            mTrs = M.insert label (fromFunction pStates f) (actions m)
                         in m {actions=mTrs}
