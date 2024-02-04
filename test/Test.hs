module Main where

import Test.QuickCheck

import System.Console.Haskeline

import qualified WorldTest
import qualified ActionsTest

tests = do
  outputStrLn "All tests passed"

main :: IO ()
main = runInputT defaultSettings (tests) >> return ()
