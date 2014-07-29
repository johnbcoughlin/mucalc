module MuCalc.MuModelTest (testList) where

import Data.Set
import MuCalc.States
import MuCalc.MuModel
import Test.HUnit hiding (State)

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

testList = [
           ]

model = newMuModel 4

{-
testRealizeProposition = forAll stateGen $ (\state ->
                           forAll (elements [0..4]) $ (\i ->
                             forAll (elements [True, False]) $ (\bool ->
                               let realization = realizeProposition i model
                                in whenFail' (putStrLn $ show realization)
                                             (contains realization (setNthElement state i bool) == bool))))
                                             -}


setNthElement :: [a] -> Int -> a -> [a]
setNthElement xs i val = fnt ++ val : bck
  where fnt = fst pair
        bck = tail (snd pair)
        pair = splitAt i xs
