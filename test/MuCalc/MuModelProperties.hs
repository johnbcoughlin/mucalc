module MuCalc.MuModelProperties (formulaProperties) where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import MuCalc.MuFormula
import MuCalc.States
import MuCalc.MuModel
import MuCalc.Realization
import MuCalc.Utils
import Test.HUnit hiding (State)
import MuCalc.Generators

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

formulaProperties = zipProperties [ ("Proposition", propositionProperty)
                                  , ("Negation", negationProperty)
                                  , ("Disjunction", disjunctionProperty)
                                  , ("Conjuction", conjunctionProperty)
                                  , ("Transitions", transitionProperty)
                                  , ("Fixpoints", fixpointProperty)
                                  ]

propositionProperty = forAll dimensions $ (\n ->
                      forAll (models n) $ (\model ->
                      forAll (dimNStates n) $ (\state ->
                      forAll (elements [0..n-1]) $ (\i ->
                      forAll (elements [True, False]) $ (\bool ->
                        let phi = Proposition i
                         in extract (realize phi model) (\set ->
                              whenFail' (putStrLn $ show set)
                                        ((set `contains` (setNthElement state i bool) `iff` bool))))))))

--Need to use forAllFormulasWithNoVariables
negationProperty = forAllModels $ (\(model, _) ->
                   forAll (negatableFormulas model) $ (\phi ->
                     extract (realize phi model) (\phiSet ->
                     extract (realize (Negation phi) model) (\notPhiSet ->
                       property $ notPhiSet == setNot phiSet))))

disjunctionProperty = forAllModels $ (\(model, _) ->
                      forAll (pairsOf (formulas model)) $ (\(phi, psi) ->
                        extract (realize phi model) (\phiSet ->
                        extract (realize psi model) (\psiSet ->
                        extract (realize (Or phi psi) model) (\unionSet ->
                          let subset = subsetN $ dimension model
                           in phiSet `subset` unionSet .&&.
                              psiSet `subset` unionSet .&&.
                              unionSet `subset` setOr phiSet psiSet)))))

conjunctionProperty = forAllModels $ (\(model, _) ->
                      forAll (pairsOf (formulas model)) $ (\(phi, psi) ->
                        extract (realize phi model) (\phiSet ->
                        extract (realize psi model) (\psiSet ->
                        extract (realize (And phi psi) model) (\intersectionSet ->
                          let subset = subsetN $ dimension model
                           in intersectionSet `subset` phiSet .&&.
                              intersectionSet `subset` psiSet .&&.
                              setAnd phiSet psiSet `subset` intersectionSet)))))

modelHasATransition = not . M.null . transitions
transitionProperty = forAllModelsSuchThat modelHasATransition $ (\(model, _) ->
                     forAll (formulas model) $ (\phi ->
                       let kv = M.findMin $ transitions model
                           k = fst kv
                           tr = snd kv
                           phiPullback = PossiblyNext k phi
                        in extract (realize phi model) (\phiSet ->
                           extract (realize phiPullback model) (\phiPullbackSet ->
                             property $ phiPullbackSet == phiSet `throughTransition` tr))))

muAtTopLevel (Mu _ _) = True
muAtTopLevel _  = False
fixpointProperty = forAllModels $ (\(model, _) ->
                   forAll ((formulas model) `suchThat` muAtTopLevel) $ (\phi ->
                     fixpointPropertyAux phi model))

fixpointPropertyAux (Mu var phi) model = extract (realize (Mu var phi) model) $ (\fixpoint ->
                                       let context = Context True (M.singleton var fixpoint)
                                        in extract (realizeAux phi context model) $(\expected ->
                                          property $ fixpoint == expected))
