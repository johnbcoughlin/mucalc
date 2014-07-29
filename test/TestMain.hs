module Main where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import MuCalc.StatesTest (testList)
import MuCalc.MuModelTest (testList)

main = defaultMain tests

tests = [ testGroup "States and Transitions" MuCalc.StatesTest.testList
        , testGroup "MuModels" MuCalc.MuModelTest.testList
        ]
