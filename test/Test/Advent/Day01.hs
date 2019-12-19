module Test.Advent.Day01 where

import Test.HUnit
import Advent

tests :: Test
tests = TestList
    [ 2 ~=? moduleFuel 12
    , 2 ~=? moduleFuel 14
    , 654 ~=? moduleFuel 1969
    , 33583 ~=? moduleFuel 100756
    -- Part 2
    , 2 ~=? moduleFuelFixed 12
    , 2 ~=? moduleFuelFixed 14
    , 966 ~=? moduleFuelFixed 1969
    , 50346 ~=? moduleFuelFixed 100756
    ]
