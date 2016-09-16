{-# LANGUAGE TemplateHaskell #-}

import           Test.QuickCheck
import           Test.QuickCheck.All

import           Test.HUnit

import           Lib
import           Puzzles.Test11
import           Puzzles.Test13
import           Puzzles.Test9

prop_one :: Int -> Spec -> Bool
prop_one i = const True

prop_two :: a -> Bool
prop_two = const True


testMinLength = TestCase (assertEqual "minLength [1,2,3]," 9 (minLength [1,2,3]))
testOther = TestCase (assertEqual "always true," True True)

unitTests = TestList
  [ TestLabel "minLength" testMinLength
  , TestLabel "other" testOther
  ]

return [] -- need this for GHC 7.8
-- quickCheckAll generates test cases for all 'prop_*' properties
main = do
  putStrLn "\n\nRunning unit tests"
  runTestTT unitTests
  putStrLn "\nRunning property tests"
  $(quickCheckAll)
