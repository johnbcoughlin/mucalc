module MuCalc.Integration (testList) where

{-
- Integration test for mu calculus realization. Represents a basic game between
- a robot and a passive environment. The robot is trying to traverse a grid with
- barriers, we are interested to know the number of squares from which a goal
- square is reachable.
-}

import Test.Framework
import Test.HUnit hiding (State)
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import MuCalc.Generators (pairsOf)
import MuCalc.Utils hiding (iff)
import MuCalc.MuFormula
import MuCalc.MuModel
import MuCalc.States
import MuCalc.Realization
import qualified Data.Set as S
import qualified Data.Map as M

testList = [ testGroup "Preliminary tests"
             [ testProperty "Position/State conversion" posStateProperty
             , testProperty "Legal moves" legalMovesProperty
             , testCase "Initial condition" initialConditionTest
             , testCase "Winning condition" winningConditionTest
             ]
           , testCase "Losing game" losingGameTest
           , testCase "Winning positions" winningPositionsTest
           ]

{-
- The game is played on a 2-d grid, so all positions can be represented as a tuple
-}
type Position = (Int, Int)
positions :: Gen Position
positions = pairsOf $ elements [0..3]

isInBounds :: Position -> Bool
isInBounds (x, y) = x >= 0 && x < 4 && y >= 0 && y < 4

posToState :: Position -> State
posToState (x, y) = if not (isInBounds (x, y))
                    then error ("Out of bounds: " ++ show (x, y))
                    else [x >= 2, x `mod` 2 == 1, y >= 2, y `mod` 2 == 1]

stateToPos :: State -> Position
stateToPos state = if not (length state == 4)
                   then error ("Wrong size state: " ++ show state)
                   else let x = (2 * if (state !! 0) then 1 else 0) + (if (state !! 1) then 1 else 0)
                            y = (2 * if (state !! 2) then 1 else 0) + (if (state !! 3) then 1 else 0)
                         in (x, y)

translateProposition :: (Position -> Bool) -> Proposition
translateProposition pred = Proposition $ pred . stateToPos

translateTransition :: (Position -> [Position]) -> Transition
translateTransition f = Transition $ (map posToState) . f . stateToPos

--Check that we're converting back and forth correctly
posStateProperty = forAll (positions) $ (\pos ->
                     (stateToPos . posToState $ pos) == pos)

{-
- Number of dimensions. The room is 4x4, so we need two bits per dimension to represent
- the robot's position. That exhausts the description of the state.
-}
dim = 4

{-
- Construct a transition. The bot is allowed to move from its current position to one
- square away, and the main diagonal of the room is blocked.
-}
move :: Position -> [Position]
move (x, y) = let unfiltered = [(x, y), (x+1, y), (x-1, y), (x, y+1), (x, y-1)]
               in filter (\pos -> isInBounds pos && isClear pos) unfiltered

isClear :: Position -> Bool
isClear (x, y) = x /= y

--Check that we're only moving to legal squares
legalMovesProperty = forAll (positions) $ (\pos ->
                       let results = move pos
                        in all (\res -> isInBounds res && isClear res) results)

{-
- Construct the model. It looks like this:
- +----+
- |X  W|
- | X  |
- |  X |
- |I  X|
- +----+
- the X's are impassable, the W is the winning square, and the I is the starting square.
-}
isInitial (0, 3) = True
isInitial _ = False
isWinning (3, 0) = True
isWinning _ = False
isPassable (x, y) = x /= y
model = (newMuModel dim) `withTransitions` (M.singleton "move" (translateTransition move))
                         `withPropositions` M.map translateProposition (M.fromList [ ("initial", isInitial)
                                                                                   , ("winning", isWinning)
                                                                                   , ("passable", isPassable)
                                                                                   ])

--The robot should start in the bottom left: pos = (0, 3), or [False, False, True, True]
initialCondition = Atom "initial"
initialConditionTest = assertRealization (realize initialCondition model) (\set ->
                         set @?= S.fromList [posToState (0, 3)])

--The robot should end up in the top right: pos = (3, 0), or [True, True, False, False]
winningCondition = Atom "winning"
winningConditionTest = assertRealization (realize winningCondition model) (\set ->
                         set @?= S.fromList [posToState (3, 0)])

clearSquare = Atom "passable"

--Eventually get to the winning condition
success = Mu "Z" (Or winningCondition
                     (PossiblyNext "move" (Variable "Z")))

--Express that we start in the initial condition and win the game
phi = initialCondition `And` success

--The game phi is not winnable.
losingGameTest = assertRealization (realize phi model) (\set ->
                   set @?= S.empty)

--If we relax the constraint that we start at I, but require that we're not on an impassable square,
--there should be six winning positions
psi = success `And` clearSquare
winningPositionsTest = assertRealization (realize psi model) (\set ->
                         set @?= S.fromList (map posToState [ (1, 0), (2, 0), (3, 0),
                                                                      (2, 1), (3, 1),
                                                                              (3, 2) ]))
