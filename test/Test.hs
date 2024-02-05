module Main where

import Test.QuickCheck

import WorldTest
import ActionsTest

main :: IO ()
main = do
    putStrLn "Running ActionsTest"
    runActionTests
