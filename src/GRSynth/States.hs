module GRSynth.States where

import OBDD
import Prelude hiding ((||), or, and, not)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits
import Data.Maybe (fromMaybe, fromJust)
import Data.Functor
import Data.List (foldl')

type PState = [Bool]

-- | Representation of a set of boolean vectors using an ordered binary
-- decision diagram. Includes a specification of the dimension of the
-- vectors.
data StateSet = Implicit { obdd :: (OBDD Int)
                         , setDim :: Maybe Int
                         }

data ExplicitStateSet = Explicit { states :: S.Set PState
                                 , explicitDim :: Int
                                 }
                                 deriving (Eq)

instance Show StateSet where
  show = show . all_models . obdd

instance Eq StateSet where
  (==) set1 set2 = (toExplicit set1) == (toExplicit set2)

--Contains no states
newBottom :: StateSet
newBottom = Implicit (constant False) Nothing

--Contains every state
newTop :: StateSet
newTop = Implicit (constant True) Nothing

--Binary and unary state set operators.
setOr :: StateSet -> StateSet -> StateSet
setOr = stateSetBinaryOp (OBDD.||)

--Intersection of two state sets
setAnd :: StateSet -> StateSet -> StateSet
setAnd = stateSetBinaryOp (OBDD.&&)

empty :: StateSet -> Bool
empty (Implicit set _) = OBDD.null set

-- | Defines a binary operator on two state sets in terms of an operator on their underlying
-- OBDD's. Requires that one or both dimensions be Nothing, or that, if they are known, they're equal.
stateSetBinaryOp :: (OBDD Int -> OBDD Int -> OBDD Int) -> StateSet -> StateSet -> StateSet
stateSetBinaryOp op set1 set2 = let obdd1 = obdd set1
                                    obdd2 = obdd set2
                                 in case setDim set1 of
                                         Nothing -> Implicit (obdd1 `op` obdd2) (setDim set2)
                                         Just n1 -> case setDim set2 of
                                                         Nothing -> Implicit (obdd1 `op` obdd2) (setDim set1)
                                                         Just n2 -> if n1 == n2
                                                                    then Implicit (obdd1 `op` obdd2) (Just n1)
                                                                    else error "Dimensions must match"


setNot :: StateSet -> StateSet
setNot set = Implicit (not $ obdd set) (setDim set)

contains :: StateSet -> PState -> Bool
contains set state = satisfiable $ foldl inject (obdd set) [0..n-1]
  where inject cur i = instantiate i (state !! i) cur
        n = maybe 0 id (setDim set)

singleton :: PState -> StateSet
singleton state = let n = length state
                      units = map (\i -> unit i (state !! i)) [0..n-1]
                   in Implicit (OBDD.and units) (Just n)

fromExplicit :: [PState] -> StateSet
fromExplicit list = foldl' (\accum state -> accum `setOr` (singleton state)) newBottom list

--Used for testing
toExplicit :: StateSet -> [PState]
toExplicit set = filter (set `contains`) (enumerateStates n)
  where n = maybe 0 id (setDim set)

{-
- Propositions are encoded as the set of states in which they are true.
-}
type PProp = StateSet

fromPredicate :: [PState] -> (PState -> Bool) -> PProp
fromPredicate domain f = let trueStates = filter f domain
                          in fromExplicit trueStates

{-
- Actions are encoded as OBDDs over two copies of the variables. We use the
- first n variables for the output, so we can easily AND the resulting OBDD with
- other OBDDs.
- See forceAction
-}
type PAction = StateSet

fromFunction :: [PState] -> (PState -> [PState]) -> PAction
fromFunction domain f = foldl setOr newBottom
                           (map (\s -> fromSingleFunctionApplication s (f s)) domain)

fromSingleFunctionApplication :: PState -> [PState] -> PAction
fromSingleFunctionApplication input output = let combinedVectors = map (++input) output
                                              in fromExplicit combinedVectors

fromRelation :: [(PState, PState)] -> PAction
fromRelation rel = let singletons = map (\(i, o) -> singleton (o ++ i)) rel
                    in foldl setOr newBottom singletons

--The set of output states reachable from the given input
processAction :: StateSet -> PAction -> StateSet
processAction phi tr = let n = (fromJust $ setDim tr) `div` 2
                           inputForce = forceAction phi newTop
                           combined = tr `setAnd` inputForce
                           justOutputs = exists_many (S.fromList [n..(2*n-1)]) (obdd combined)
                        in Implicit justOutputs (Just n)

--The set of states from which a phi-state is reachable through the given PAction
throughAction :: StateSet -> PAction -> StateSet
throughAction phi tr = rebase $ forceAction phi tr

--Force an action to map onto phi-states.
--This is done by taking the intersection of valid (output, input) tuples with the set of desired phi-outputs.
forceAction :: StateSet -> PAction -> PAction
forceAction phi tr = tr `setAnd` (phi { setDim = (*2) <$> setDim phi })

forcedByAction :: StateSet -> PAction -> StateSet
forcedByAction phi tr = setNot $ throughAction (setNot phi) tr

--Whoo, power sets. This should be pretty efficient with laziness.
enumerateStates :: Int -> [PState]
enumerateStates dim = let cardinality = 2^dim::Int
                          ints = [0..cardinality-1]
                          toBitList n = map (testBit n) [0..dim-1]
                        in map toBitList ints

rebase :: PAction -> StateSet
rebase tr = let n = maybe 0 (`div` 2) (setDim tr)
                justInputs = exists_many (S.fromList [0..n-1]) (obdd tr)
                hashes = all_models justInputs
                stateList = concatMap (rebaseMapToState n) hashes
             in fromExplicit stateList

rebaseMapToState :: Int -> M.Map Int Bool -> [PState]
rebaseMapToState n hash = let allStates = enumerateStates n
                              matchesAtEveryIndex state = all (\i ->
                                                      (state !! i) == (fromMaybe
                                                                        (state !! i)
                                                                        (M.lookup (i+n) hash)))
                                                      [0..n-1]
                           in filter matchesAtEveryIndex allStates
