module MuCalc.States where

import OBDD
import Prelude hiding ((||), or, and, not)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bits
import Data.Maybe (fromMaybe)

type PState = [Bool]

data StateSet = Implicit { obdd :: (OBDD Int)
                         , setDim :: Int
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

contains :: StateSet -> PState -> Bool
contains set state = satisfiable $ foldl inject (obdd set) [0..n-1]
  where inject cur i = instantiate i (state !! i) cur
        n = setDim set

singleton :: PState -> StateSet
singleton state = let n = length state
                      units = map (\i -> unit i (state !! i)) [0..n-1]
                   in Implicit (OBDD.and units) n

fromExplicit :: ExplicitStateSet -> StateSet
fromExplicit set = S.foldl' (\accum state -> accum `setOr` (singleton state))
                                   (newBottom (explicitDim set))
                                   (states set)

--Used for testing
toExplicit :: StateSet -> ExplicitStateSet
toExplicit set = Explicit (S.fromList $ filter (set `contains`) (enumerateStates n)) n
  where n = setDim set

{-
- Propositions are encoded as the set of states in which they are true.
-}
type PhysicalProposition = StateSet

predicateToPhysicalProposition :: Int -> (PState -> Bool) -> PhysicalProposition
predicateToPhysicalProposition n f = let allStates = enumerateStates n
                                         trueStates = S.fromList (filter f allStates)
                                         explicit = Explicit trueStates n
                                      in fromExplicit explicit

{-
- Actions are encoded as OBDDs over two copies of the variables. We use the
- first n variables for the output, so we can easily AND the resulting OBDD with
- other OBDDs.
- See forceAction
-}
type PAction = StateSet

fanoutToPAction :: Int -> (PState -> [PState]) -> PAction
fanoutToPAction n f = let explicit s = Explicit (S.fromList (f s)) n
                                  in fromFunction n explicit

fromFunction :: Int -> (PState -> ExplicitStateSet) -> PAction
fromFunction dim f = foldl setOr (newBottom (dim * 2))
                           (map (\s -> fromSingleFunctionApplication s (f s) dim) (enumerateStates dim))

fromSingleFunctionApplication :: PState -> ExplicitStateSet -> Int -> PAction
fromSingleFunctionApplication input output dim = let combinedVectors = S.map (++input) (states output)
                                                  in fromExplicit (Explicit combinedVectors dim)

--The set of states from which a phi-state is reachable through the given PAction
throughAction :: StateSet -> PAction -> StateSet
throughAction phi tr = rebase $ forceAction phi tr

--Force an action to map onto phi-states.
--This is done by taking the intersection of valid (output, input) tuples with the set of desired phi-outputs.
forceAction :: StateSet -> PAction -> PAction
forceAction phi tr = tr `setAnd` phi

--Whoo, power sets. This should be pretty efficient with laziness.
enumerateStates :: Int -> [PState]
enumerateStates dim = let cardinality = 2^dim::Int
                          ints = [0..cardinality-1]
                          toBitList n = map (testBit n) [0..dim-1]
                        in map toBitList ints

rebase :: PAction -> StateSet
rebase tr = let n = (setDim tr) `div` 2
                justInputs = exists_many (S.fromList [0..n-1]) (obdd tr)
                hashes = all_models justInputs
                stateList = concatMap (rebaseMapToState n) hashes
                explicit = Explicit (S.fromList stateList) n
             in fromExplicit explicit

rebaseMapToState :: Int -> M.Map Int Bool -> [PState]
rebaseMapToState n hash = let allStates = enumerateStates n
                              matchesAtEveryIndex state = all (\i ->
                                                      (state !! i) == (fromMaybe
                                                                        (state !! i)
                                                                        (M.lookup (i+n) hash)))
                                                      [0..n-1]
                           in filter matchesAtEveryIndex allStates
