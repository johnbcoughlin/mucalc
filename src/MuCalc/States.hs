module MuCalc.States where

import OBDD
import Prelude hiding ((||), or, and, not)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits
import Data.Maybe (fromMaybe)

type State = [Bool]

data StateSet = Implicit { obdd :: (OBDD Int)
                         , setDim :: Int
                         }

data ExplicitStateSet = Explicit { states :: S.Set State
                                 , explicitDim :: Int
                                 }
                                 deriving (Eq)

instance Show StateSet where
  show = show . all_models . obdd

instance Eq StateSet where
  (==) set1 set2 = (toExplicit set1) == (toExplicit set2)

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
                      units = map (\i -> (unit i (state !! i))) [0..n-1]
                   in Implicit (OBDD.and units) n

fromExplicit :: ExplicitStateSet -> StateSet
fromExplicit set = S.foldl' (\accum -> (\state -> accum `setOr` (singleton state)))
                                   (newBottom (explicitDim set))
                                   (states set)

--Used for testing
toExplicit :: StateSet -> ExplicitStateSet
toExplicit set = Explicit (S.fromList $ filter (set `contains`) (enumerateStates n)) n
  where n = setDim set

{-
- Transitions are encoded as OBDDs over two copies of the variables. We use the
- first n variables for the output, so we can easily AND the resulting OBDD with
- other OBDDs.
- See forceTransition
-}
type PhysicalTransition = StateSet

predicateToPhysicalTransition :: Int -> (State -> Bool) -> PhysicalTransition
predicateToPhysicalTransition n f = let allStates = enumerateStates n
                                        trueStates = S.fromList (filter f allStates)
                                        explicit = Explicit trueStates n
                                     in fromExplicit explicit

fanoutToPhysicalTransition :: Int -> (State -> [State]) -> PhysicalTransition
fanoutToPhysicalTransition n f = let explicit = (\s -> Explicit (S.fromList (f s)) n)
                                  in fromFunction n explicit

fromFunction :: Int -> (State -> ExplicitStateSet) -> PhysicalTransition
fromFunction dim f = foldl (\accum -> (\transition -> accum `setOr` transition))
                           (newBottom (dim * 2))
                           (map (\s -> fromSingleFunctionApplication s (f s) dim) (enumerateStates dim))

fromSingleFunctionApplication :: State -> ExplicitStateSet -> Int -> PhysicalTransition
fromSingleFunctionApplication input output dim = let combinedVectors = S.map (++input) (states output)
                                                  in fromExplicit (Explicit combinedVectors dim)

--The set of states from which a phi-state is reachable through the given PhysicalTransition
throughTransition :: StateSet -> PhysicalTransition -> StateSet
throughTransition phi tr = rebase $ forceTransition phi tr

--Force a transition to map onto phi-states.
--This is done by taking the intersection of valid (output, input) tuples with the set of desired phi-outputs.
forceTransition :: StateSet -> PhysicalTransition -> PhysicalTransition
forceTransition phi tr = tr `setAnd` phi

--Whoo, power sets. This should be pretty efficient with laziness.
enumerateStates :: Int -> [State]
enumerateStates dim = let cardinality = 2^dim::Int
                          ints = [0..cardinality-1]
                          toBitList = (\n -> map (testBit n) [0..dim-1])
                        in map toBitList ints

rebase :: PhysicalTransition -> StateSet
rebase tr = let n = (setDim tr) `div` 2
                justInputs = exists_many (S.fromList [0..n-1]) (obdd tr)
                hashes = all_models justInputs
                stateList = concatMap (rebaseMapToState n) hashes
                explicit = Explicit (S.fromList stateList) n
             in fromExplicit explicit

rebaseMapToState :: Int -> M.Map Int Bool -> [State]
rebaseMapToState n hash = let allStates = enumerateStates n
                              matchesAtEveryIndex = (\state -> all (\i ->
                                                      (state !! i) == (fromMaybe
                                                                        (state !! i)
                                                                        (M.lookup (i+n) hash)))
                                                    [0..n-1])
                           in filter matchesAtEveryIndex allStates
