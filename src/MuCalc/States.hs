module MuCalc.States where

import OBDD
import Prelude hiding ((||), or, and, not)
import Data.Map hiding (map, foldl, singleton, findMin, fromList)
import Data.Set hiding (foldl, singleton)
import Data.Bits
import Data.Maybe (fromMaybe)

type State = [Bool]

data StateSet = Implicit { obdd :: (OBDD Int)
                         , setDim :: Int
                         }

data ExplicitStateSet = Explicit { states :: Set State
                                 , explicitDim :: Int
                                 }
                                 deriving (Eq)

instance Show StateSet where
  show = show . all_models . obdd

--Contains no states
newBottom :: Int -> StateSet
newBottom = Implicit (constant False)

--Contains every state
newTop :: Int -> StateSet
newTop = Implicit (constant True)

--Binary and unary state set operators. They all preserve the dimension of the first argument.
--Union of two state sets
setOr :: StateSet -> StateSet -> StateSet
setOr set1 set2 = Implicit (obdd set1 || obdd set2) (setDim set1)

--Intersection of two state sets
setAnd :: StateSet -> StateSet -> StateSet
setAnd set1 set2 = Implicit (obdd set1 OBDD.&& obdd set2) (setDim set1)

setNot :: StateSet -> StateSet
setNot set = Implicit (not $ obdd set) (setDim set)

contains :: StateSet -> State -> Bool
contains set state = satisfiable $ foldl inject (obdd set) [0..n-1]
  where inject = \cur -> \i -> instantiate i (state !! i) cur
        n = setDim set

singleton :: State -> StateSet
singleton state = let n = length state
                      units = Prelude.map (\i -> (unit i (state !! i))) [0..n-1]
                   in Implicit (OBDD.and units) n

fromExplicit :: ExplicitStateSet -> StateSet
fromExplicit set = Data.Set.foldl' (\accum -> (\state -> accum `setOr` (singleton state)))
                                   (newBottom (explicitDim set))
                                   (states set)

--Used for testing
toExplicit :: StateSet -> ExplicitStateSet
toExplicit set = Explicit (fromList $ Prelude.filter (set `contains`) (enumerateStates n)) n
  where n = setDim set

{-
- Transitions are encoded as OBDDs over two copies of the variables. We use the
- first n variables for the output, so we can easily AND the resulting OBDD with
- other OBDDs.
- See forceTransition
-}
type Transition = StateSet

fromFunction :: Int -> (State -> ExplicitStateSet) -> Transition
fromFunction dim f = foldl (\accum -> (\transition -> accum `setOr` transition))
                           (newBottom (dim * 2))
                           (Prelude.map (\s -> fromSingleFunctionApplication s (f s) dim) (enumerateStates dim))

fromSingleFunctionApplication :: State -> ExplicitStateSet -> Int -> Transition
fromSingleFunctionApplication input output dim = let combinedVectors = Data.Set.map (++input) (states output)
                                                  in fromExplicit (Explicit combinedVectors dim)

--The set of states from which a phi-state is reachable through the given Transition
throughTransition :: StateSet -> Transition -> StateSet
throughTransition phi tr = rebase $ forceTransition phi tr

--Force a transition to map onto phi-states.
--This is done by taking the intersection of valid (output, input) tuples with the set of desired phi-outputs.
forceTransition :: StateSet -> Transition -> Transition
forceTransition phi tr = tr `setAnd` phi

--Whoo, power sets. This should be pretty efficient with laziness.
enumerateStates :: Int -> [State]
enumerateStates dim = let cardinality = 2^dim::Int
                          ints = [0..cardinality-1]
                          toBitList = (\n -> Prelude.map (testBit n) [0..dim-1])
                        in Prelude.map toBitList ints

rebase :: Transition -> StateSet
rebase tr = let n = (setDim tr) `div` 2
                justInputs = exists_many (fromList [0..n-1]) (obdd tr)
                hashes = all_models justInputs
                stateList = concatMap (rebaseMapToState n) hashes
                explicit = Explicit (fromList stateList) n
             in fromExplicit explicit

rebaseMapToState :: Int -> Map Int Bool -> [State]
rebaseMapToState n hash = let allStates = enumerateStates n
                              matchesAtEveryIndex = (\state -> Prelude.all (\i ->
                                                      (state !! i) == (fromMaybe
                                                                        (state !! i)
                                                                        (Data.Map.lookup (i+n) hash)))
                                                    [0..n-1])
                           in Prelude.filter matchesAtEveryIndex allStates
