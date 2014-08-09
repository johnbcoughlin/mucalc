module GRSynth.GameStructureTest (testList) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 ()
import GRSynth.GameStructure
import qualified GRSynth.FormulasTest as F

testList :: [Test]
testList = [ testGroup "Simple formulas" F.testList
           ]

