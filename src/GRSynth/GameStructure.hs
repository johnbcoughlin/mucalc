module GRSynth.GameStructure where

import qualified Data.Map as M
import MuCalc.States
import GRSynth.Formulas
import GRSynth.Semantics

data (State x, State y) => GameStructure x y = GameStructure { dX :: [x]
                                                             , dY :: [y]
                                                             , props :: M.Map String PProp
                                                             , theta :: SimpleFormula
                                                             , env :: PAction
                                                             , sys :: PAction
                                                             , assumptions :: [SimpleFormula]
                                                             , guarantees :: [SimpleFormula]
                                                             }

newGameStructure :: (State x, State y) => GameStructure x y
newGameStructure = error "not implemented: newGameStructure"

-- | Adds a named atomic proposition to the given GameStructure. The proposition
-- is specified as a predicate on complete (input, output) states.
withPropAsPredicate :: (State x, State y) =>
    GameStructure x y -> String -> ((x, y) -> Bool) -> GameStructure x y
withPropAsPredicate = error "not implemented: withPropAsPredicate"

withPropAsSupport :: (State x, State y) =>
    GameStructure x y -> String -> [(x, y)] -> GameStructure x y
withPropAsSupport = error "not implemented: withPropAsSupport"

withInitialSatisfying :: (State x, State y) =>
    GameStructure x y -> String -> ((x, y) -> Bool) -> GameStructure x y
withInitialSatisfying = error "not implemented: withThetaSatisfying"

withInitialStates :: (State x, State y) =>
    GameStructure x y -> String -> [(x, y)] -> GameStructure x y
withInitialStates = error "not implemented: withInitialStates"

withEnvActionAsFunction :: (State x, State y) =>
    GameStructure x y -> String -> ((x, y) -> [x]) -> GameStructure x y
withEnvActionAsFunction = error "not implemented: withEnvActionAsFunction"

withEnvActionAsRelation :: (State x, State y) =>
    GameStructure x y -> String -> [((x, y), x)] -> GameStructure x y
withEnvActionAsRelation = error "not implemented: withEnvActionAsFunction"

withSysActionAsFunction :: (State x, State y) =>
    GameStructure x y -> String -> ((x, y) -> [x]) -> GameStructure x y
withSysActionAsFunction = error "not implemented: withSysActionAsFunction"

withSysActionAsRelation :: (State x, State y) =>
    GameStructure x y -> String -> [((x, y), (x, y))] -> GameStructure x y
withSysActionAsRelation = error "not implemented: withSysActionAsRelation"

withAssumption :: (State x, State y) =>
    GameStructure x y -> SimpleFormula -> GameStructure x y
withAssumption = error "not implemented: withAssumption"

withGuarantee :: (State x, State y) =>
    GameStructure x y -> SimpleFormula -> GameStructure x y
withGuarantee = error "not implemented: withGuarantee"
