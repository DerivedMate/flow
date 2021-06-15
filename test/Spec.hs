module Main where

import Test.HUnit
import SSyntax


main :: IO Counts
main = do runTestTT syntaxTests

