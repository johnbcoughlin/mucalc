module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 ()
import MuCalc.StatesTest (testList)
import MuCalc.MuModelTest (testList)
import MuCalc.Integration
import GRSynth.GameStructureTest
import Test.QuickCheck ()

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "States and Transitions" MuCalc.StatesTest.testList
        , testGroup "MuModels" MuCalc.MuModelTest.testList
        , testGroup "Integration" MuCalc.Integration.testList
        , testGroup "Synthesis" Synthesis.GameStructureTest.testList
        ]
