{-# Language OverloadedStrings, FlexibleContexts #-}
module Day01 where

import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

moduleFuel :: Int -> Int
moduleFuel mass =
    truncate (fromIntegral mass / 3) - 2

tests :: Bool
tests =
  all (uncurry run) testData
  where
    run mass expected = moduleFuel mass == expected
    testData = 
      [ (12, 2)
      , (14, 2)
      , (1969, 654)
      , (100756, 33583)
      ]

type Error = String -- sorry

getTotalFuel :: T.Text -> Either Error Int
getTotalFuel input =
   let
        readMass t = case T.decimal t of
            Left s -> throwError s
            Right (m, "") -> pure m
            Right (_, rest) -> throwError . T.unpack $ "Incomplete parse: " <> rest
        totalFuel = fmap (sum . map moduleFuel) . traverse readMass . T.words
    in totalFuel input


main :: IO ()
main =
    let total = getTotalFuel <$> T.readFile "input.txt"
    in total >>= print
