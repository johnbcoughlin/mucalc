module MuCalc.Integration where

{-
- Integration test for mu calculus realization. Represents a basic game between
- a robot and a passive environment. The robot is trying to traverse a grid with
- barriers, we are interested to know the number of squares from which a goal
- square is reachable.
-}

import MuCalc.MuFormula

{-
- Number of dimensions. The room is 4x4, so we need two bits per dimension to represent
- the robot's position. That exhausts the description of the state.
-}
dim = 4
