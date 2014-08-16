module GRSynth.GameStructure where

import qualified Data.Map as M
import GRSynth.States
import GRSynth.Formulas
import GRSynth.Semantics

data (State x, State y) => GS x y = GS { dX :: [x]
                                       , dY :: [y]
                                       , dom :: [(x, y)]
                                       , gsprops :: M.Map String PProp
                                       , gstheta :: SimpleFormula
                                       , gsenv :: PAction
                                       , gssys :: PAction
                                       , gsassumptions :: [SimpleFormula]
                                       , gsguarantees :: [SimpleFormula]
                                       }

data GameStructure = GameStructure { props :: M.Map String PProp
                                   , theta :: SimpleFormula
                                   , env :: PAction
                                   , sys :: PAction
                                   , assumptions :: [SimpleFormula]
                                   , guarantees :: [SimpleFormula]
                                   }

finalize :: (State x, State y) => GS x y -> GameStructure
finalize (GS _ _ _ p t e s a g) = GameStructure p t e s a g

newGameStructure :: (State x, State y) => [x] -> [y] -> GS x y
newGameStructure xs ys = let xys = [(x, y) | x <- xs, y <- ys]
                          in GS { dX = xs, dY = ys, dom = xys, gsprops = M.empty
                                , gsassumptions = [], gsguarantees = [] }

-- | Adds a named atomic proposition to the given GS. The proposition
-- is specified as a predicate on complete (input, output) states.
withPropAsPredicate :: (State x, State y) => GS x y -> String -> ((x, y) -> Bool) -> GS x y
withPropAsPredicate gs label p = let pred (Just (s, _)) = p s
                                     pred _ = False
                                     pDom = map encode $ dom gs
                                     pProp = fromPredicate pDom (pred . decode)
                                  in gs { gsprops = M.insert label pProp (gsprops gs) }

withPropAsSupport :: (State x, State y) => GS x y -> String -> [(x, y)] -> GS x y
withPropAsSupport gs label xys = let pProp = fromExplicit (map encode xys)
                                  in gs { gsprops = M.insert label pProp (gsprops gs) }

withInitialState :: (State x, State y) => GS x y -> SimpleFormula -> GS x y
withInitialState gs f = gs { gstheta=f }

withEnvActionAsFunction :: (State x, State y) => GS x y -> ((x, y) -> [x]) -> GS x y
withEnvActionAsFunction gs f = let action (Just ((x, y), _)) = [(x', y) | x' <- f (x, y)]
                                   action _ = []
                                in gs { gsenv = actionToTransition gs action }

withEnvActionAsRelation :: (State x, State y) => GS x y -> [((x, y), x)] -> GS x y
withEnvActionAsRelation gs rel = let fullRel = [((x, y), (x', y)) | ((x, y), x') <- rel]
                                     pRel = map (\(i, o) -> (encode i, encode o)) fullRel
                                     transition = fromRelation pRel
                                  in gs { gsenv = transition }

withSysActionAsFunction :: (State x, State y) => GS x y -> ((x, y) -> [(x, y)]) -> GS x y
withSysActionAsFunction gs f = let action (Just (s, _)) = f s
                                   action _ = []
                                in gs { gssys = actionToTransition gs action }

withSysActionAsRelation :: (State x, State y) => GS x y -> [((x, y), (x, y))] -> GS x y
withSysActionAsRelation gs rel = let pRel = map (\(i, o) -> (encode i, encode o)) rel
                                     transition = fromRelation pRel
                                  in gs { gssys = transition }

withAssumption :: (State x, State y) => GS x y -> SimpleFormula -> GS x y
withAssumption gs f = gs { gsassumptions = f : gsassumptions gs }

withGuarantee :: (State x, State y) => GS x y -> SimpleFormula -> GS x y
withGuarantee gs f = gs { gsguarantees = f : gsguarantees gs }

actionToTransition :: (State x, State y) => GS x y -> (Maybe ((x, y), PState) -> [(x, y)]) -> PAction
actionToTransition gs action = let pAction = map encode . action . decode
                                   pDom = map encode $ dom gs
                                in fromFunction pDom pAction
