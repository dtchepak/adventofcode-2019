{-# Language OverloadedStrings, FlexibleContexts #-}
module Day01 where

import Control.Monad.Except
import Data.List (unfoldr)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

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

type Error = String -- sorry

getTotalFuel :: (Int -> Int) -> T.Text -> Either Error Int
getTotalFuel fuelCalc input =
   let
        readMass t = case T.decimal t of
            Left s -> throwError s
            Right (m, "") -> pure m
            Right (_, rest) -> throwError . T.unpack $ "Incomplete parse: " <> rest
        totalFuel = fmap (sum . map fuelCalc) . traverse readMass . T.words
    in totalFuel input

moduleFuelFixed :: Int -> Int
moduleFuelFixed mass =
    let initialFuel = moduleFuel mass
        additionalFuel m = if m<=0 then Nothing else Just (m, moduleFuel m)
    in sum . unfoldr additionalFuel $ initialFuel

main :: IO ()
main = do
    input <- T.readFile "input.txt"
    let total1 = getTotalFuel moduleFuel input
    let total2 = getTotalFuel moduleFuelFixed input
    print (total1, total2)
