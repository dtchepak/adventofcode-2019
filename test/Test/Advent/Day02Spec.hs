module Test.Advent.Day02Spec (spec) where

import Advent
import Test.Hspec

spec :: Spec
spec = do
  let emptyProgram = toIntCode []
  specify "crash on empty program" $ do
    step (Running 100 emptyProgram) `shouldBe` Crash "Invalid reg: 100 (program count: 0)" emptyProgram
  specify "example step" $
    do
      step (Running 0 (toIntCode [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]))
      `shouldBe` Running 4 (toIntCode [1, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50])
