{-# LANGUAGE ScopedTypeVariables #-}

module GRSynth.Semantics ( State, encode, decode
                         )
                         where

import Prelude hiding (lookup)
import OBDD hiding ((&&), (||), not)
import qualified Data.Map as M
import GRSynth.States
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
  -- | Tries to decode part of a boolean vector and return the result, plus
  -- the remainder of the vector.
  decode :: PState -> Maybe (s, PState)

instance (State x, State y) => State (x, y) where
  encode (x, y) = let px = encode x
                      py = encode y
                   in px ++ py
  decode pState = do (x, rest) <- decode pState
                     (y, []) <- decode rest --if decoding y doesn't use the entire vector, fail.
                     return ((x, y), [])
