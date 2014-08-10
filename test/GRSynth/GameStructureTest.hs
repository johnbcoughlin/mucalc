{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GRSynth.GameStructureTest (testList) where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Functor
import Control.Applicative
import Test.Framework
import Test.HUnit hiding (State, Test)
import Test.Framework.Providers.HUnit
import GRSynth.GameStructure
import GRSynth.Semantics
import GRSynth.States
import GRSynth.Utils
import qualified GRSynth.FormulasTest as F

testList :: [Test]
testList = [ testGroup "Simple formulas" F.testList
           , testGroup "Game structure constructors" constructorTests
           ]

constructorTests = [ testCase "With prop as predicate" withPropAsPredTest
                   , testGroup "With prop as support" withPropAsSupportTests
                   , testGroup "With env action as function" withEnvAsFuncTests
                   , testGroup "With env action as relation" withEnvAsRelationTests
                   , testGroup "With sys action as function" withSysAsFuncTests
                   , testGroup "With sys action as relation" withSysAsRelationTests
                   ]

withPropAsPredTest = let pred (Int2 x, Int2 y) = x + y == 2
                         pred _ = False
                         gs = baseGS `withPropAsPredicate` "A" $ pred
                         pStates = toExplicit . fromJust $ M.lookup "A" (props gs)
                      in interpret pStates @?~ [(Int2 0, Int2 2), (Int2 1, Int2 1), (Int2 2, Int2 0)]

withPropAsSupportTest list = let gs = baseGS `withPropAsSupport` "A" $ list
                                 pStates = toExplicit . fromJust $ M.lookup "A" (props gs)
                              in interpret pStates @?~ list

withPropAsSupportTests = [ testCase "Normal" (withPropAsSupportTest [(Int2 3, Int2 0), (Int2 1, Int2 2)])
                         , testCase "Empty" (withPropAsSupportTest [])
                         , testCase "One" (withPropAsSupportTest [(Int2 0, Int2 0)])
                         ]

goalStates = map encode [(Int2 0, Int2 0), (Int2 1, Int2 2)]
empty1 = [] :: [Int2]
empty2 = [] :: [(Int2, Int2)]
empty3 = [] :: [((Int2, Int2), Int2)]
empty4 = [] :: [((Int2, Int2), (Int2, Int2))]
withEnvAsFuncTest f expected = let gs = baseGS `withEnvActionAsFunction` f
                                   startingStates = toExplicit (fromExplicit goalStates `throughAction` env gs)
                                in interpret startingStates @?~ expected

withEnvAsFuncTests = [ testCase "Empty" $ withEnvAsFuncTest (const empty1) empty2
                     , testCase "Right constant" $ withEnvAsFuncTest (const [Int2 1])
                     [(Int2 0,Int2 2), (Int2 1,Int2 2), (Int2 2,Int2 2), (Int2 3,Int2 2)]
                     , testCase "Wrong constant" $ withEnvAsFuncTest (const [Int2 2]) empty2
                     , testCase "Partial" $ withEnvAsFuncTest
                     (\(Int2 x, _) -> [Int2 (if x==0 then 3 else x-1)])
                     [(Int2 1, Int2 0), (Int2 2, Int2 2)]
                     ]

withEnvAsRelationTest rel expected = let gs = baseGS `withEnvActionAsRelation` rel
                                         startingStates = toExplicit (fromExplicit goalStates `throughAction` env gs)
                                      in interpret startingStates @?~ expected
withEnvAsRelationTests = [ testCase "Empty" $ withEnvAsRelationTest empty3 empty2
                         , testCase "Just one" $ withEnvAsRelationTest
                         [((Int2 3, Int2 0), Int2 0)] [(Int2 3, Int2 0)]
                         , testCase "Wrong value" $ withEnvAsRelationTest
                         [((Int2 3, Int2 0), Int2 2)] empty2
                         , testCase "Wrong X value" $ withEnvAsRelationTest
                         [((Int2 3, Int2 3), Int2 0)] empty2
                         , testCase "Both possibilities" $ withEnvAsRelationTest
                         [((Int2 3, Int2 0), Int2 0), ((Int2 3, Int2 2), Int2 1)]
                         [(Int2 3, Int2 0), (Int2 3, Int2 2)]
                         ]

withSysAsFuncTest f expected = let gs = baseGS `withSysActionAsFunction` f
                                   startingStates = toExplicit (fromExplicit goalStates `throughAction` sys gs)
                                in interpret startingStates @?~ expected

withSysAsFuncTests = [ testCase "Empty" $ withSysAsFuncTest (const empty2) empty2
                     , testCase "Right constant" $ withSysAsFuncTest (const [(Int2 1, Int2 2)]) cartProd
                     , testCase "Wrong constant" $ withSysAsFuncTest (const [(Int2 3, Int2 3)]) empty2
                     , testCase "Partial" $ withSysAsFuncTest
                     (\(Int2 x, Int2 y) -> [(Int2 0, Int2 0) | x + y == 2])
                     [(Int2 0, Int2 2), (Int2 1, Int2 1), (Int2 2, Int2 0)]
                     ]

withSysAsRelationTest rel expected = let gs = baseGS `withSysActionAsRelation` rel
                                         startingStates = toExplicit (fromExplicit goalStates `throughAction` sys gs)
                                      in interpret startingStates @?~ expected

withSysAsRelationTests = [ testCase "Empty" $ withSysAsRelationTest empty4 empty2
                         , testCase "Just one" $ withSysAsRelationTest
                         [((Int2 3, Int2 3), (Int2 0, Int2 0))] [(Int2 3, Int2 3)]
                         , testCase "Just input" $ withSysAsRelationTest
                         [((Int2 3, Int2 3), (Int2 0, Int2 3))] empty2
                         , testCase "Both possibilities" $ withSysAsRelationTest
                         [((Int2 3, Int2 3), (Int2 0, Int2 0)), ((Int2 3, Int2 3), (Int2 1, Int2 2))] [(Int2 3, Int2 3)]
                         ]
