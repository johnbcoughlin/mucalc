module GRSynth.GameStructure where

import qualified Data.Map as M
import MuCalc.States
import GRSynth.Formulas
import GRSynth.Semantics

data (State x, State y) => GameStructure x y = GameStructure { dX :: [x]
                                                             , dY :: [y]
                                                             , dom :: [(x, y)]
                                                             , props :: M.Map String PProp
                                                             , theta :: SimpleFormula
                                                             , env :: PAction
                                                             , sys :: PAction
                                                             , assumptions :: [SimpleFormula]
                                                             , guarantees :: [SimpleFormula]
                                                             }

newGameStructure :: (State x, State y) => [x] -> [y] -> GameStructure x y
newGameStructure xs ys = let xys = [(x, y) | x <- xs, y <- ys]
                          in GameStructure { dX = xs, dY = ys, dom = xys, props = M.empty }

-- | Adds a named atomic proposition to the given GameStructure. The proposition
-- is specified as a predicate on complete (input, output) states.
withPropAsPredicate :: (State x, State y) =>
    GameStructure x y -> String -> ((x, y) -> Bool) -> GameStructure x y
withPropAsPredicate gs label p = let pred (Just (s, _)) = p s
                                     pred _ = False
                                     pDom = map encode $ dom gs
                                     pProp = fromPredicate pDom (pred . decode)
                                  in gs { props = M.insert label pProp (props gs) }

withPropAsSupport :: (State x, State y) =>
    GameStructure x y -> String -> [(x, y)] -> GameStructure x y
withPropAsSupport gs label xys = let pProp = fromExplicit (map encode xys)
                                  in gs { props = M.insert label pProp (props gs) }

withInitialState :: (State x, State y) =>
    GameStructure x y -> SimpleFormula -> GameStructure x y
withInitialState gs f = gs { theta=f }

withEnvActionAsFunction :: (State x, State y) => GameStructure x y -> ((x, y) -> [x]) -> GameStructure x y
withEnvActionAsFunction gs f = let action (Just (s, _)) = [(x, y) | x <- f s, y <- dY gs]
                                   action _ = []
                                   pAction = map encode . action . decode
                                   pDom = map encode $ dom gs
                                   transition = fromFunction pDom pAction
                                in gs { env = transition }

withEnvActionAsRelation :: (State x, State y) => GameStructure x y -> [((x, y), x)] -> GameStructure x y
withEnvActionAsRelation gs rel = let fullRel = [(s, (x, y)) | (s, x) <- rel, y <- dY gs]
                                     pRel = map (\(i, o) -> (encode i, encode o)) fullRel
                                     transition = fromRelation pRel
                                  in gs { env = transition }

withSysActionAsFunction :: (State x, State y) => GameStructure x y -> ((x, y) -> [(x, y)]) -> GameStructure x y
withSysActionAsFunction gs f = let action (Just (s, _)) = f s
                                   action _ = []
                                   pAction = map encode . action . decode
                                   pDom = map encode $ dom gs
                                   transition = fromFunction pDom pAction
                                in gs { sys = transition }

withSysActionAsRelation :: (State x, State y) => GameStructure x y -> [((x, y), (x, y))] -> GameStructure x y
withSysActionAsRelation gs rel = let pRel = map (\(i, o) -> (encode i, encode o)) rel
                                     transition = fromRelation pRel
                                  in gs { sys = transition }

withAssumption :: (State x, State y) =>
    GameStructure x y -> SimpleFormula -> GameStructure x y
withAssumption = error "not implemented: withAssumption"

withGuarantee :: (State x, State y) =>
    GameStructure x y -> SimpleFormula -> GameStructure x y
withGuarantee = error "not implemented: withGuarantee"
