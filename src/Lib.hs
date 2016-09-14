{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Lib
    ( Spec
    , solveAndPrint
    ) where

import Data.Array
import Data.List

data Pixel = Black | White deriving (Eq)
type Row = [Maybe Pixel]
type Puzzle = [Row]
type Spec = [Int]
type Perm = [Int]
type RowPerms = [[Perm]]
type State = (RowPerms, RowPerms, Puzzle)

instance Show Pixel where
  show Black = "##"
  show White = ".."

instance Show Row where
  show = concatMap showPixel
    where
      showPixel (Just p) = show p
      showPixel Nothing = "``"


minLength :: Spec -> Int
minLength ls = sum ls + length ls - 1


{-
All posible permutations of a Spec in a row of the given width

Searches depth first.

Example:

ls = [1,2,3]
width = 10

perms ls width =
  [ [1,3,6]   x.xx.xxx..
  , [1,3,7]   x.xx..xxx.
  , [1,3,8]   x.xx...xxx
  , [1,4,7]   x..xx.xxx.
  , [1,4,8]   x..xx..xxx
  , [1,5,8]   x...xx.xxx
  , [2,4,7]   .x.xx.xxx.
  , [2,4,8]   .x.xx..xxx
  , [2,5,8]   .x..xx.xxx
  , [3,5,8]   ..x.xx.xxx
  ]
-}
perms :: Int -> Spec -> [Perm]
perms width spec =
  perms' spec 1
  where
    perms' :: Spec -> Int -> [Perm]
    perms' (x:xs) i =
      do
        j <- [i..limit]  -- try all possible 'white' offsets
        perm <- perms' xs (j + x + 1) -- continue depth first
        return (j : perm)
      where
        limit = width - x + 1
    perms' [] _ = [[]]


isBlack :: Int -> Spec -> Perm -> Bool
isBlack i spec perm = any (inRange i) $ perm `zip` spec
  where inRange i (j, x) = i >= j && i < j + x


isWhite :: Int -> Spec -> Perm -> Bool
isWhite i spec perm = not (isBlack i spec perm)


-- filter out permutations that can't be applied anymore
filterPerms :: Spec -> [Perm] -> Row -> [Perm]
filterPerms spec perms row =
  foldr filterPixel perms $ row `zip` [1..]
  where
    filterPixel (Just Black, i) p = filter (isBlack i spec) p
    filterPixel (Just White, i) p = filter (isWhite i spec) p
    filterPixel (Nothing, _) p = p


-- calculate probabilities for black/white for a row
rowProbs :: Spec -> [Perm] -> Row -> [(Int, Int)]
rowProbs spec perms row =
  map determineProbs $ row `zip` [1..]
  where
    determineProbs (Just Black, _) = (1, 0)
    determineProbs (Just White, _) = (0, 1)
    determineProbs (Nothing, i) = (blacks, length perms - blacks)
      where blacks = length . filter (isBlack i spec) $ perms


-- try to fill each pixel in row if it is Nothing and
-- all permutations lead to the same color
fillRow :: Spec -> [Perm] -> Row -> Row
fillRow spec perms row =
  map determineColor $ row `zip` [1..]
  where
    determineColor (Just p, _) = Just p
    determineColor (Nothing, i)
      | all (isBlack i spec) perms = Just Black
      | all (isWhite i spec) perms = Just White
      | otherwise = Nothing


-- do a row based sweep on the puzzle
-- returns the filtered permutations and the new puzzle
sweepRows :: [Spec] -> (RowPerms, Puzzle) -> (RowPerms, Puzzle)
sweepRows spec (rowPerms, puzzle) =
  unzip (map sweepRow $ zip3 spec rowPerms puzzle)
  where
    sweepRow (spec, perms, row) =
      let
        perms' = filterPerms spec perms row
        row' = fillRow spec perms' row
      in
        (perms', row')


initState :: ([Spec], [Spec]) -> State
initState (specH, specV) =
  (initPermsH, initPermsV, initPuzzle)
  where
    width = length specV
    height = length specH
    initPuzzle = replicate height (replicate width Nothing)
    initPermsH = map (perms width) specH
    initPermsV = map (perms height) specV


-- solve a puzzle
solveStep :: ([Spec], [Spec]) -> State -> State
solveStep (specH, specV)
    = transposePuzzle
    . sweepRows' specV
    . transposePuzzle
    . sweepRows' specH
  where
    -- same as sweepRows but keeps the vertical RowPerms (v) constant
    sweepRows' :: [Spec] -> State -> State
    sweepRows' spec (h, v, puzzle) = (h', v, puzzle')
      where (h', puzzle') = sweepRows spec (h, puzzle)

    -- flip H and V
    transposePuzzle :: State -> State
    transposePuzzle (h, v, puzzle) = (v, h, transpose puzzle)


solve :: ([Spec], [Spec]) -> State -> State
solve spec state@(_, _, puzzle) =
  let
    state'@(_, _, puzzle') = solveStep spec state
  in
    if puzzle == puzzle' then
      state'
    else
      solve spec state'


solveWithLookahead :: ([Spec], [Spec]) -> Int -> State -> (State, Int)
solveWithLookahead spec lookahead state =
  let
    state'@(h, v, puzzle) = solve spec state
  in
    if isSolved puzzle || lookahead <= 0 then
      (state, lookahead)
    else
      -- look for a spec with the least permutations
      -- try each one and see if only one leads to a valid state
      solveWithLookahead spec (lookahead - 1) state
      where
        rowPermsWithLength r = [1..] `zip` map (\p -> (length p, p)) r
        leastPerms r = minimumBy (\a b -> compare (snd a) (snd b)) $ rowPermsWithLength r


isSolved :: Puzzle -> Bool
isSolved = all (all notNothing)
  where
    notNothing (Just p) = True
    notNothing Nothing = False



printPuzzle :: Puzzle -> IO ()
printPuzzle = mapM_ print


printState :: State -> IO ()
printState (h, v, puzzle) =
  do
    if solved then
      putStrLn "Solved!"
    else
      do
        putStrLn "Unsolvable :( permutations left:"
        putStrLn ("H: " ++ hl)
        putStrLn ("V: " ++ vl)
    putStrLn ""
    printPuzzle puzzle
  where
    solved = all (== 1) (map length h) && all (== 1) (map length v)
    hl = intercalate "," $ map (show . length) h
    vl = intercalate "," $ map (show . length) v


solveAndPrint :: ([Spec], [Spec]) -> IO ()
solveAndPrint spec = printState (solve spec (initState spec))


test9_specH :: [Spec]
test9_specH =
  [ [8]
  , [4,4]
  , [2,2,1]
  , [2,1,1,1,2,3]
  , [1,2,1,1]
  -- --
  , [2,6,2]
  , [1,1,2,2,2,1]
  , [1,2,6,1,1]
  , [3,3]
  , [5,1,2,1,3]
  -- --
  , [5,2,1,2]
  , [1,7,4]
  , [2,10]
  , [3,4]
  , [7]
  ]

test9_specV :: [Spec]
test9_specV =
  [ [5]
  , [3,3]
  , [2,4]
  , [2,2,2,1]
  , [1,1,1,2,2]
  -- --
  , [1,2,2,1]
  , [2,1,1,1,2]
  , [1,1,3,2,1]
  , [1,1,1,3,1]
  , [1,1,1,3,1]
  -- --
  , [1,2,1,1,2,1]
  , [1,1,1,1,1,2,1]
  , [1,3,2,3]
  , [2,1,1,2]
  , [1,2,1,3]
  -- --
  , [1,2,3]
  , [3,1,1,2]
  , [1,4]
  , [3,3]
  , [4]
  ]

test9_spec = (test9_specH, test9_specV)


test11_specH :: [Spec]
test11_specH =
  [ [5,11]
  , [1,1,2,2]
  , [3,1,13]
  , [1,1,1,1,1]
  , [1,3,1,1,2,1]
  -- --
  , [1,1,1,3,1,1,1,1]
  , [5,3,1,1,1,1,1]
  , [1,1,1,1,4]
  , [1,13]
  , [1,2,5,2]
  -- --
  , [1,2,1,1,2]
  , [1,13]
  , [1,12]
  , [3,3,3,3]
  , [7,3,3]
  ]

test11_specV :: [Spec]
test11_specV =
  [ [2]
  , [7,2]
  , [1,1,1,2]
  , [1,1,1,7,1]
  , [1,1,1,2]
  -- --
  , [7,2]
  , [2]
  , [11]
  , [3,1,7]
  , [1,1,2,1,4]
  -- --
  , [1,1,1,1,4]
  , [1,1,8]
  , [1,1,2,2]
  , [1,8,2]
  , [1,1,2,2]
  -- --
  , [1,1,2,5]
  , [1,1,1,2,4]
  , [1,1,5,4]
  , [3,8]
  , [12]
  ]

test11_spec = (test11_specH, test11_specV)


test :: IO ()
test =
  do
    print x
    print (minLength x)
    print (perms 10 x)
  where
    x = [1,2,3]
