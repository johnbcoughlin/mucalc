module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 ()
import GRSynth.StatesTest (testList)
import GRSynth.GameStructureTest
import GRSynth.SynthesisTest
import Test.QuickCheck ()

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "States and Transitions" GRSynth.StatesTest.testList
        , testGroup "Game structures" GRSynth.GameStructureTest.testList
        , testGroup "Synthesis" GRSynth.SynthesisTest.testList
        ]
