{-# Language OverloadedStrings, FlexibleContexts #-}
module Advent.Day01 where

import Control.Monad.Except
import Data.List (unfoldr)
import qualified Advent.Day01Input as Input

moduleFuel :: Int -> Int
moduleFuel mass =
    truncate (fromIntegral mass / 3) - 2

type TestError = String -- sorry
tests :: Either TestError [()]
tests =
  traverse run testData
  where
    run (mass, expected, expectedFix) =
        let (actualM, actualM') = (moduleFuel mass, moduleFuelFixed mass)
        in if actualM == expected && actualM' == expectedFix
        then pure ()
        else throwError . mconcat $
            [ "For ", show mass
            , " Expected: ", show (expected, expectedFix)
            , ", Actual: ", show (actualM, actualM') ]
    testData = 
      [ (12, 2, 2)
      , (14, 2, 2)
      , (1969, 654, 966)
      , (100756, 33583, 50346)
      ]

type Mass = Int -- sorry

getTotalFuel :: (Int -> Int) -> [Mass] -> Int
getTotalFuel fuelCalc =
    sum . map fuelCalc

moduleFuelFixed :: Mass -> Int
moduleFuelFixed mass =
    let initialFuel = moduleFuel mass
        additionalFuel m = if m<=0 then Nothing else Just (m, moduleFuel m)
    in sum . unfoldr additionalFuel $ initialFuel

main :: IO ()
main = 
    let total1 = getTotalFuel moduleFuel Input.input
        total2 = getTotalFuel moduleFuelFixed Input.input
    in print (total1, total2)
