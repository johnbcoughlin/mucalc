module Main where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import MuCalc.StatesTest (testList)
import MuCalc.MuModelTest (testList)
import MuCalc.Integration
import MuCalc.Generators
import Test.QuickCheck

main = defaultMain tests

tests = [ testGroup "States and Transitions" MuCalc.StatesTest.testList
        --, testGroup "MuModels" MuCalc.MuModelTest.testList
        , testGroup "Integration" MuCalc.Integration.testList
        ]
