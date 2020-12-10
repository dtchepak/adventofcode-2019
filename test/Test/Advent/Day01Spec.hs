module Test.Advent.Day01Spec (spec) where

import Advent
import Test.Hspec

spec :: Spec
spec = do
    specify "examples" $ do
      2 `shouldBe` moduleFuel 12
      2 `shouldBe` moduleFuel 14
      654 `shouldBe` moduleFuel 1969
      33583 `shouldBe` moduleFuel 100756
      -- Part 2
      2 `shouldBe` moduleFuelFixed 12
      2 `shouldBe` moduleFuelFixed 14
      966 `shouldBe` moduleFuelFixed 1969
      50346 `shouldBe` moduleFuelFixed 100756
