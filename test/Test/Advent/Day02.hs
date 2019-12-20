module Test.Advent.Day02 where

import Test.HUnit
import Advent

tests :: Test
tests =
    let emptyProgram = toIntCode []
    in TestList
      [
        Crash "Invalid reg: 100 (program count: 0)" emptyProgram ~=? step (Running 100 emptyProgram),
        Running 4 (toIntCode [1,9,10,70,2,3,11,0,99,30,40,50])
            ~=? step (Running 0 (toIntCode [1,9,10,3,2,3,11,0,99,30,40,50]))
      ]
