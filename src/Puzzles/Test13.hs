module Puzzles.Test13 (test13_spec) where

import           Lib

test13_specH :: [Spec]
test13_specH =
  [ [3,1]
  , [2,2]
  , [2,4]
  , [2,3,1]
  , [3,8]
  -- --
  , [18]
  , [13,3]
  , [13]
  , [13]
  , [12,1]
  -- --
  , [4,2,3]
  , [3,2,2,2,2]
  , [3,2,2,3,1]
  , [2,1,2,2,4]
  , [3,2,2,2]
  ]

test13_specV :: [Spec]
test13_specV =
  [ [4]
  , [9,3]
  , [2,11]
  , [1,8,1]
  , [1,7]
  -- --
  , [6,2]
  , [5,4]
  , [5,1,1]
  , [5]
  , [5]
  -- --
  , [9]
  , [11]
  , [8,2]
  , [8,1]
  , [6,3]
  -- --
  , [1,3,3]
  , [4,2]
  , [3,4]
  , [1,2]
  , [2]
  ]

test13_spec = (test13_specH, test13_specV)