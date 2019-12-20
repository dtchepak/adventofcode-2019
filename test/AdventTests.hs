module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Test.Advent.Day01 as D01
import qualified Test.Advent.Day02 as D02

main :: IO ()
main =
  defaultMain . hUnitTestToTests $
       TestList
        [ TestLabel "Day01" D01.tests
        , TestLabel "Day02" D02.tests
        ]
