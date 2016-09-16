{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( Spec
    , Perm
    , minLength
    , perms
    , isBlack
    , isWhite
    , solveAndPrint
    , probs
    ) where

import           Data.List
import           Data.Ratio

data Pixel = Black | White deriving (Eq)
type Row = [Maybe Pixel]
type Puzzle = [Row]
type Spec = [Int]
type Perm = [Int]
type RowPerms = [[Perm]]
type State = (RowPerms, RowPerms, Puzzle)
type Pos = (Int, Int)
type Prob = (Int, Int)

instance Show Pixel where
  show Black = "##"
  show White = ".."

instance {-# OVERLAPPING #-} Show Row where
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
  if not (null perms') then perms' else perms
  where
    perms' = foldr filterPixel perms $ row `zip` [1..]
    filterPixel (Just Black, i) p = filter (isBlack i spec) p
    filterPixel (Just White, i) p = filter (isWhite i spec) p
    filterPixel (Nothing, _) p = p


-- calculate probabilities for black/white for a row
rowProbs :: Spec -> [Perm] -> Row -> [Prob]
rowProbs spec perms row =
  zipWith (curry determineProbs) row [1..]
  where
    determineProbs (Just Black, _) = (1, 0)
    determineProbs (Just White, _) = (0, 1)
    determineProbs (Nothing, i) = (blacks, length perms - blacks)
      where blacks = length . filter (isBlack i spec) $ perms


flattenProbs :: [Prob] -> Row
flattenProbs = map flatten
  where
    flatten (_, 0) = Just Black
    flatten (0, _) = Just White
    flatten _ = Nothing


-- try to fill each pixel in row if it is Nothing and
-- all permutations lead to the same color
fillRow :: Spec -> [Perm] -> Row -> Row
fillRow spec perms = flattenProbs . rowProbs spec perms


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


-- calculate the probability for (black, white) for each pixel
probs :: ([Spec], [Spec]) -> State -> [[(Int, Int)]]
probs (specH, specV) state@(rowPermsH, rowPermsV, puzzle) =
  zipWith (zipWith sumProbs) hProbs vProbs
  where
    xProbs spec rowPerms puzzle =
      map (uncurry3 rowProbs) $ zip3 spec rowPerms puzzle
    hProbs = xProbs specH rowPermsH puzzle
    vProbs = transpose (xProbs specV rowPermsV (transpose puzzle))

    sumProbs :: (Int, Int) -> (Int, Int) -> (Int, Int)
    sumProbs (a, b) (c, d) = (a + b, c + d)

    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f (x,y,z) = f x y z


probsToRatio :: [[Prob]] -> [[Ratio Int]]
probsToRatio = map (map (\(a, b) -> a % b))


-- lookup the best probability (except the determined ones) in the puzzle
bestProb :: [[Prob]] -> (Pos, Prob)
bestProb probs =
  maximumBy maxProb $ map bestInRow $ probs `zip` [1..]
  where
    bestInRow :: ([Prob], Int) -> (Pos, Prob)
    bestInRow (row, j) =
      let (prob, i) = maximumBy maxProb $ row `zip` [1..]
      in ((i, j), prob)

    probRatio :: Prob -> Ratio Int
    probRatio (b, w)
      | b > w = b % (b + w)
      | otherwise = w % (b + w)

    maxProb (a, _) (b, _) = maxProb' (probRatio a) (probRatio b)
    maxProb' a b
      | a == 1 = LT -- a is determined, pick b
      | b == 1 = GT -- b is determined, pick a
      | otherwise = a `compare` b


replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i e lst = take i lst ++ [e] ++ drop (i + 1) lst


setPixel :: Puzzle -> Pos -> Pixel -> Puzzle
setPixel puzzle (i, j) pixel =
  replaceAtIndex (j - 1) newRow puzzle
  where
    oldRow = puzzle !! (j - 1)
    newRow = replaceAtIndex (i - 1) (Just pixel) oldRow


solveWithLookahead :: ([Spec], [Spec]) -> Int -> State -> (Bool, State, Int)
solveWithLookahead spec lookahead state =
  let
    state'@(h, v, puzzle) = solve spec state
    solved = isSolved puzzle
  in
    if solved || lookahead <= 0 then
      (False, state, lookahead)
    else
      let
        (pos, (b, w)) = bestProb (probs spec state')
        setPixel' = setPixel puzzle pos
        puzzleBlack = setPixel' Black
        puzzleWhite = setPixel' White
        (puzzleA, puzzleB)
          | b > w = (puzzleBlack, puzzleWhite)
          | otherwise = (puzzleWhite, puzzleBlack)
        solve' p = solveWithLookahead spec (lookahead - 1) (h, v, p)
        resA@(solvedA, _, _) = solve' puzzleA
        resB = solve' puzzleB
      in
        if solvedA then resA else resB


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

test :: IO ()
test =
  do
    print x
    print (minLength x)
    print (perms 10 x)
  where
    x = [1,2,3]
