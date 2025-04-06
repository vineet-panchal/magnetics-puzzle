-- Name: Vineet Panchal
-- Student Number: 501238284
-- Course: CPS506
-- Section Number: 021
-- Date: April 6, 2025

module Polarity (polarity) where

import qualified Data.Map as Map  -- for the key-value lookups
import qualified Data.Set as Set  -- for tracking used positions
import Data.List (sortBy)  -- for sorting lists
import Data.Ord (comparing) -- for creating comparison functions
-- importing necessary libraries

polarity :: [String] -> ([Int], [Int], [Int], [Int]) -> [String]
polarity board specs = 
-- main polarity function to solve the puzzle
-- board: array of strings representing the board
-- specs: tuple of integer arrays representing constraints
-- the function returns an array of strings representing the solved board

  let
    height = length board  -- number of rows in the board
    width = if height > 0 then length (head board) else 0  -- number of columns in the board
    -- get the dimensions of the board
        
    (left, right, top, bottom) = specs  
    -- getting the constraints from the specs tuple, and assigning them to the variables left, right, top, and bottom
        
    magnetPositions = findMagnetPositions board height width  
    -- find all the possible positions where the magnets can be placed, using the board, the height, and the width 
        
    posMaps = createPosMaps magnetPositions height width
    -- create position maps for easy lookups, using the magnet positions and the height
        
    sortedPositions = sortByConstraints magnetPositions (height, width, left, right, top, bottom)
    -- sort the magnet positions based on their constraints, using the height, width, and the constraints

    -- Create empty solution board filled with 'X' placeholders
    initialBoard = replicate height (replicate width 'X')
        
    initialPositiveRowCount = replicate height 0  -- count of the positive poles in each row
    initialNegativeRowCount = replicate height 0  -- count of the negative poles in each row
    initialPositiveColCount = replicate width 0  -- count of the positive poles in each column
    initialNegativeColCount = replicate width 0  -- count of the negative poles in each column
    -- initializing the counters to track number of positive and negative poles in each row and column

    initialUsedPositions = Set.empty  -- set to track used positions
        
    maxCounts = findMaxCounts (height, width, left, right, top, bottom)
    -- calculating the maximum allowed positive and negative poles for each row and column, using the height, width, and constraints
        
    -- Solve the puzzle using backtracking algorithm starting with the first position
    result = solveWithPositions 
      sortedPositions  -- list of positions sorted by priority
      initialBoard  -- the initial empty board 
      (height, width, left, right, top, bottom)  -- the board dimensions and constraints
      maxCounts  -- maximum allowed poles in each row and column 
      posMaps  -- maps for easy position lookups
      0  -- first position
      initialPositiveRowCount  -- initial count of positive poles in rows
      initialNegativeRowCount  -- initial count of negative poles in rows
      initialPositiveColCount  -- initial count of positive poles in columns
      initialNegativeColCount  -- initial count of negative poles in columns
      initialUsedPositions  -- set of used positions
    -- solve the puzzle using backtracking, starting with the first position and the initial board
  in
    -- Convert the result to the required output format
    case result of
    -- cases of the result to return
      Just solvedBoard -> map (map (\c -> if c == 'X' then 'X' else c)) solvedBoard  -- return the solution
      Nothing -> replicate height (replicate width 'X')  -- return the empty board if no solution is found

findMagnetPositions :: [String] -> Int -> Int -> [((Int, Int), (Int, Int), String)]
findMagnetPositions board height width = 
-- function findMagnetPositions to find all the valid positions where magnets can be placed on the board
-- board: array of strings representing the board
-- height: number of rows in the board
-- width: number of columns in the board
-- the function returns a list of tuples representing the positions and orientations of the magnets

  [((row, col), (row, col+1), "horizontal") | row <- [0..height-1], col <- [0..width-2], col+1 < width, board !! row !! col == 'L'] ++
  -- find horizontal magnet positions - R (right) cells indicate right side of horizontal magnets
  [((row, col), (row+1, col), "vertical") | row <- [0..height-2], col <- [0..width-1], row+1 < height, board !! row !! col == 'T']
  -- find vertical magnet positions - T (top) cells indicate top side of vertical magnets

createPosMaps :: [((Int, Int), (Int, Int), String)] -> Int -> Int -> 
                  (Map.Map Int [((Int, Int), (Int, Int), String)], 
                  Map.Map Int [((Int, Int), (Int, Int), String)],
                  Map.Map Int [((Int, Int), (Int, Int), String)],
                  Map.Map Int [((Int, Int), (Int, Int), String)])
createPosMaps positions height width =
-- function createPosMaps to create maps for efficient lookups of positions affecting each row and column
-- positions: list of tuples representing the positions of the magnets
-- height: number of rows in the board
-- width: number of columns in the board
-- the function returns a tuple of four maps: one for rows, one for columns, one for left and right

  let
    initialPosRowMap = Map.fromList [(i, []) | i <- [0..height-1]]  -- maps rows to positions that could place a positve pole in that row
    initialNegRowMap = Map.fromList [(i, []) | i <- [0..height-1]]  -- maps rows to positions that could place a negative pole in that row
    initialPosColMap = Map.fromList [(i, []) | i <- [0..width-1]]  -- maps columns to positions that could place a positive pole in that column
    initialNegColMap = Map.fromList [(i, []) | i <- [0..width-1]]  -- maps columns to positions that could place a negative pole in that column 
    -- initializing the maps to empty lists for each row and column
    
    updateMaps (posRowMap, negRowMap, posColMap, negColMap) pos@((row1, col1), (row2, col2), _) =
      ( Map.adjust (\xs -> ((row1, col1), (row2, col2), "") : xs) row1 posRowMap  -- position can add a positive pole to row1
      , Map.adjust (\xs -> ((row1, col1), (row2, col2), "") : xs) row2 negRowMap  -- position can add a negative pole to row2
      , Map.adjust (\xs -> ((row1, col1), (row2, col2), "") : xs) col1 posColMap  -- position can add a positive pole to col1
      , Map.adjust (\xs -> ((row1, col1), (row2, col2), "") : xs) col2 negColMap )  -- position can add a negative pole to col2
    -- function to update the maps with a new position
    
  in
    foldl updateMaps (initialPosRowMap, initialNegRowMap, initialPosColMap, initialNegColMap) positions
    -- foldl function to apply the update function to each position in the list of positions

sortByConstraints :: [((Int, Int), (Int, Int), String)] -> (Int, Int, [Int], [Int], [Int], [Int]) -> [((Int, Int), (Int, Int), String)]
sortByConstraints positions (_, _, left, right, top, bottom) =
-- function sortByConstraints to sort the magnet positions based on their constraints
-- positions: list of tuples representing the positions of the magnets
-- left, right, top, bottom: lists of rows and columns that are constrained by the positions
-- the function returns a sorted list of positions based on their constraints

  let
    scorePosition position@((row1, col1), (row2, col2), _) =
    -- calculating the score for each position based on its constraints
      let
        addScore score True val = score + val  -- add to score if the condition is true
        addScore score False val = score  -- don't add to the score if the condition is false
        -- helper function to add points to the score based on the condition        

        score = 0  -- initializing score

        score1 = addScore score (left !! row1 /= -1) 2  -- add points if row1 has a positive constraint 
        score2 = addScore score1 (right !! row1 /= -1) 2  -- add points if row1 has a negative constraint 
        score3 = addScore score2 (top !! col1 /= -1) 2  -- add points if col1 has a positive constraint  
        score4 = addScore score3 (bottom !! col1 /= -1) 2  -- add points if col1 has a negative constraint 
        score5 = addScore score4 (left !! row2 /= -1) 2  -- add points if row2 has a positive constraint 
        score6 = addScore score5 (right !! row2 /= -1) 2  -- add points if row2 has a negative constraint 
        score7 = addScore score6 (top !! col2 /= -1) 2  -- add points if col2 has a positive constraint  
        score8 = addScore score7 (bottom !! col2 /= -1) 2  -- add points if col2 has a negative constraint 
        -- adding poitns for each constraint affecting this position

        score9 = addScore score8 (left !! row1 `elem` [0, 1]) 3
        score10 = addScore score9 (right !! row1 `elem` [0, 1]) 3
        score11 = addScore score10 (top !! col1 `elem` [0, 1]) 3
        score12 = addScore score11 (bottom !! col1 `elem` [0, 1]) 3
        score13 = addScore score12 (left !! row2 `elem` [0, 1]) 3
        score14 = addScore score13 (right !! row2 `elem` [0, 1]) 3
        score15 = addScore score14 (top !! col2 `elem` [0, 1]) 3
        score16 = addScore score15 (bottom !! col2 `elem` [0, 1]) 3
        -- adding points for each tight constraint affecting this position
                
        score17 = addScore score16 (left !! row1 == right !! row1 && left !! row1 /= -1) 5
        score18 = addScore score17 (left !! row2 == right !! row2 && left !! row2 /= -1) 5
        score19 = addScore score18 (top !! col1 == bottom !! col1 && top !! col1 /= -1) 5
        finalScore = addScore score19 (top !! col2 == bottom !! col2 && top !! col2 /= -1) 5
        -- adding points for matching constraints
      in
        (position, finalScore)  -- return the position and its score
        
    scoredPositions = map scorePosition positions 
    -- calculate the score for each position
    -- map the scorePosition function to each position in the list of positions
  in
    map fst (sortBy (flip (comparing snd)) scoredPositions)
    -- sort the positions based on their scores in descending order

findMaxCounts :: (Int, Int, [Int], [Int], [Int], [Int]) -> ([Int], [Int], [Int], [Int])
findMaxCounts (height, width, left, right, top, bottom) =
-- function findMaxCounts to calculate the maximum allowed poles for each row and column
-- constraints: left, right, top, bottom
-- the function returns a tuple of four lists: positive row max, negative row max, positive column max, negative column max

  let
    positiveRowMax = [if l == -1 then width else l | l <- left]
    -- for rows where there is no negative requirement, use width as max poles
        
    negativeRowMax = [if r == -1 then width else r | r <- right]
    -- for the rows where there is no positive requirement, use width as max poles
        
    positiveColMax = [if t == -1 then height else t | t <- top]
    -- for the columns where there is no negative requirement, use height as max poles

    negativeColMax = [if b == -1 then height else b | b <- bottom]
    -- for the columns where there is no positive requirement, use height as max poles
  in
    (positiveRowMax, negativeRowMax, positiveColMax, negativeColMax)
    -- return the max counts for each row and column

areConstraintsWithinLimits :: (Int, Int, [Int], [Int], [Int], [Int]) -> ([Int], [Int], [Int], [Int]) -> 
                              [Int] -> [Int] -> [Int] -> [Int] -> Bool
areConstraintsWithinLimits (height, width, left, right, top, bottom) (posRowMax, negRowMax, posColMax, negColMax)
                          posRowCount negRowCount posColCount negColCount =
-- function areConstraintsWithinLimits to check if the current counts are within the limits defined by constraints
-- constraints: left, right, top, bottom
-- posRowMax, negRowMax, posColMax, negColMax: lists of maximum allowed poles for each row and column
-- posRowCount, negRowCount, posColCount, negColCount: current counts of poles
-- the function returns True if the current counts are within the limits, False otherwise

  let
    areRowsValid = all (\row -> 
      (left !! row == -1 || posRowCount !! row <= left !! row) &&  -- check if the positive row count is within constraints
      (right !! row == -1 || negRowCount !! row <= right !! row) &&  -- check if the negative row count is within constraints
      posRowCount !! row <= posRowMax !! row &&  -- check if the positive row count is within the maximum
      negRowCount !! row <= negRowMax !! row  -- check if the negative row count is within the maximum
      ) [0..height-1]
    -- check if any row constraints are violated

    areColsValid = all (\col ->
      (top !! col == -1 || posColCount !! col <= top !! col) &&  -- check if the positive column count is within constraints
      (bottom !! col == -1 || negColCount !! col <= bottom !! col) &&  -- check if the negative column count is within constraints
      posColCount !! col <= posColMax !! col &&  -- check if the positive column count is within the maximum
      negColCount !! col <= negColMax !! col  -- check if the negative column count is within the maximum
      ) [0..width-1]
    -- check if any column constraints are violated
  in
    areRowsValid && areColsValid  -- return True if all constraints are satisfied, False otherwise

solveWithPositions :: [((Int, Int), (Int, Int), String)] -> [[Char]] -> 
                      (Int, Int, [Int], [Int], [Int], [Int]) -> ([Int], [Int], [Int], [Int]) ->
                      (Map.Map Int [((Int, Int), (Int, Int), String)], 
                      Map.Map Int [((Int, Int), (Int, Int), String)],
                      Map.Map Int [((Int, Int), (Int, Int), String)],
                      Map.Map Int [((Int, Int), (Int, Int), String)]) ->
                      Int -> [Int] -> [Int] -> [Int] -> [Int] -> Set.Set (Int, Int) -> Maybe [[Char]]
solveWithPositions positions solution boardInfo maxCounts posMaps
                    posIndex posRowCount negRowCount posColCount negColCount usedPos =
-- function solveWithPositions to recursively try placing magnets at different positions
-- positions: list of tuples representing the positions of the magnets
-- solution: current state of the board
-- boardInfo: tuple of board dimensions and constraints
-- maxCounts: tuple of maximum allowed poles for each row and column
-- posMaps: maps for easy position lookups
-- posIndex: current index of the position being tried
-- posRowCount, negRowCount, posColCount, negColCount: current counts of poles
-- usedPos: set of used positions
-- the function returns Just solution if a solution is found, Nothing otherwise

  if not (areConstraintsWithinLimits boardInfo maxCounts posRowCount negRowCount posColCount negColCount)
  then Nothing  -- check if any constraints are violated, if so, return Nothing
  else if posIndex >= length positions  -- check if all the positions have been tried
  then
    if constraintsSatisfied solution boardInfo posRowCount negRowCount posColCount negColCount
    -- check if all the constraints are satisfied
    then Just solution  -- solution found
        else Nothing  -- we tried all positions but didn't find a solution
    else
      let
        ((row1, col1), (row2, col2), _) = positions !! posIndex
        -- get the current position from the list of positions
      in
        if Set.member (row1, col1) usedPos || Set.member (row2, col2) usedPos
        then solveWithPositions positions solution boardInfo maxCounts posMaps
              (posIndex + 1) posRowCount negRowCount posColCount negColCount usedPos
        -- if either position is already used, skip to the next position
        else
          case tryPlacingMagnet positions solution boardInfo maxCounts posMaps
                posIndex posRowCount negRowCount posColCount negColCount 
                usedPos (row1, col1) (row2, col2) '+' '-' of
          -- try placing the magnet with + on first cell and - on second
            Just result -> Just result  -- solution found with +- orientation
            Nothing ->
              case tryPlacingMagnet positions solution boardInfo maxCounts posMaps
                    posIndex posRowCount negRowCount posColCount negColCount 
                    usedPos (row1, col1) (row2, col2) '-' '+' of
              -- try placing the magnet with - on first cell and + on second
                Just result -> Just result  -- solution found with -+ orientation
                Nothing -> solveWithPositions positions solution boardInfo maxCounts posMaps (posIndex + 1) posRowCount negRowCount posColCount negColCount usedPos
                -- no solution found with either orientation, skip to the next position

tryPlacingMagnet :: [((Int, Int), (Int, Int), String)] -> [[Char]] -> 
                    (Int, Int, [Int], [Int], [Int], [Int]) -> ([Int], [Int], [Int], [Int]) ->
                    (Map.Map Int [((Int, Int), (Int, Int), String)], 
                    Map.Map Int [((Int, Int), (Int, Int), String)],
                    Map.Map Int [((Int, Int), (Int, Int), String)],
                    Map.Map Int [((Int, Int), (Int, Int), String)]) ->
                    Int -> [Int] -> [Int] -> [Int] -> [Int] -> Set.Set (Int, Int) -> 
                    (Int, Int) -> (Int, Int) -> Char -> Char -> Maybe [[Char]]
tryPlacingMagnet positions solution boardInfo maxCounts posMaps
                posIndex posRowCount negRowCount posColCount negColCount usedPos pos1@(row1, col1) pos2@(row2, col2) pole1 pole2 =
-- function tryPlacingMagnet to try placing a magnet at a specific position with given polarities
-- positions: list of tuples representing the positions of the magnets
-- solution: current state of the board
-- boardInfo: tuple of board dimensions and constraints
-- maxCounts: tuple of maximum allowed poles for each row and column
-- posMaps: maps for easy position lookups
-- posIndex: current index of the position being tried
-- posRowCount, negRowCount, posColCount, negColCount: current counts of poles
-- usedPos: set of used positions
-- pos1, pos2: positions of the magnets
-- pole1, pole2: polarities of the magnets
-- the function returns Just solution if a solution is found, Nothing otherwise

  if canPlaceMagnet solution boardInfo maxCounts pos1 pos2 pole1 pole2 posRowCount negRowCount posColCount negColCount
  -- check if the magnet can be placed at the given positions
  then
    let
      (newSol, newPosRow, newNegRow, newPosCol, newNegCol) = placeMagnet solution pos1 pos2 pole1 pole2 posRowCount negRowCount posColCount negColCount
      -- place the magnet on the board and update the counts

      newUsed = Set.insert pos1 (Set.insert pos2 usedPos)
      -- mark the positions as used in the set
    in
      solveWithPositions positions newSol boardInfo maxCounts posMaps (posIndex + 1) newPosRow newNegRow newPosCol newNegCol newUsed
      -- continue solving with the next position
  else
    Nothing  -- cannot place the magnet, return Nothing

placeMagnet :: [[Char]] -> (Int, Int) -> (Int, Int) -> Char -> Char -> [Int] -> [Int] -> [Int] -> [Int] -> ([[Char]], [Int], [Int], [Int], [Int])
placeMagnet solution (row1, col1) (row2, col2) pole1 pole2 posRowCount negRowCount posColCount negColCount =
-- function placeMagnet to place a magnet on the board and update the counts of poles
-- solution: current state of the board
-- (row1, col1), (row2, col2): positions of the magnets
-- pole1, pole2: polarities of the magnets
-- posRowCount, negRowCount, posColCount, negColCount: current counts of poles
-- the function returns the updated board and counts of poles

  let
    newRow1 = take col1 (solution !! row1) ++ [pole1] ++ drop (col1 + 1) (solution !! row1)
    newSol1 = take row1 solution ++ [newRow1] ++ drop (row1 + 1) solution
    -- update the solution board with second pole
        
    newRow2 = take col2 (newSol1 !! row2) ++ [pole2] ++ drop (col2 + 1) (newSol1 !! row2)
    newSol = take row2 newSol1 ++ [newRow2] ++ drop (row2 + 1) newSol1
    -- update the solution board with first pole
        
    tempPosRow = updateCount posRowCount row1 (pole1 == '+')  -- update first row if pole1 is positive
    newPosRow = updateCount tempPosRow row2 (pole2 == '+')  -- update second row if pole2 is positive
    -- update positive row counts
        
    tempNegRow = updateCount negRowCount row1 (pole1 == '-')  -- update first row if pole1 is negative
    newNegRow = updateCount tempNegRow row2 (pole2 == '-')  -- update second row if pole2 is negative
    -- update negative row counts
        
    tempPosCol = updateCount posColCount col1 (pole1 == '+')  -- update first column if pole1 is positive
    newPosCol = updateCount tempPosCol col2 (pole2 == '+')  -- update second column if pole2 is positive
    -- update positive column counts
        
    tempNegCol = updateCount negColCount col1 (pole1 == '-')  -- update first column if pole1 is negative
    newNegCol = updateCount tempNegCol col2 (pole2 == '-')  -- update second column if pole2 is negative
    -- update negative column counts
  in
    (newSol, newPosRow, newNegRow, newPosCol, newNegCol)  -- return updated board and counts

updateCount :: [Int] -> Int -> Bool -> [Int]
updateCount counts index shouldIncrement =
-- function updateCount to update a count in a list at a specific index
-- counts: list of counts
-- index: index of the count to be updated
-- shouldIncrement: boolean indicating whether to increment the count
-- the function returns the updated list of counts

    if shouldIncrement  
    then take index counts ++ [counts !! index + 1] ++ drop (index + 1) counts  -- increment the count at index
    else counts  -- leave counts unchanged if shouldIncrement is False

canPlaceMagnet :: [[Char]] -> (Int, Int, [Int], [Int], [Int], [Int]) -> ([Int], [Int], [Int], [Int]) -> 
                  (Int, Int) -> (Int, Int) -> Char -> Char ->
                  [Int] -> [Int] -> [Int] -> [Int] -> Bool
canPlaceMagnet solution (height, width, left, right, top, bottom) (posRowMax, negRowMax, posColMax, negColMax)
                (row1, col1) (row2, col2) pole1 pole2 posRowCount negRowCount posColCount negColCount =
-- function canPlaceMagnet to check if a magnet can be placed at a specific position
-- solution: current state of the board
-- (row1, col1), (row2, col2): positions of the magnets
-- pole1, pole2: polarities of the magnets
-- boardInfo: tuple of board dimensions and constraints
-- maxCounts: tuple of maximum allowed poles for each row and column
-- posRowCount, negRowCount, posColCount, negColCount: current counts of poles
-- the function returns True if the magnet can be placed, False otherwise

    if row1 < 0 || row1 >= height || col1 < 0 || col1 >= width || row2 < 0 || row2 >= height || col2 < 0 || col2 >= width
    -- check if the positions are out of bounds
    then False
    else  -- check if cells are empty
      let
        cell1Empty = solution !! row1 !! col1 == 'X'
        cell2Empty = solution !! row2 !! col2 == 'X'
      in
        if not cell1Empty || not cell2Empty
        then False
        else  -- calculate the potential new counts after placing this magnet
          let
            testPosRow1 = if pole1 == '+' then posRowCount !! row1 + 1 else posRowCount !! row1
            testPosRow2 = if pole2 == '+' then posRowCount !! row2 + 1 else posRowCount !! row2
            testPosCol1 = if pole1 == '+' then posColCount !! col1 + 1 else posColCount !! col1
            testPosCol2 = if pole2 == '+' then posColCount !! col2 + 1 else posColCount !! col2
            -- calculate new positive pole counts in rows and columns

            testNegRow1 = if pole1 == '-' then negRowCount !! row1 + 1 else negRowCount !! row1
            testNegRow2 = if pole2 == '-' then negRowCount !! row2 + 1 else negRowCount !! row2
            testNegCol1 = if pole1 == '-' then negColCount !! col1 + 1 else negColCount !! col1
            testNegCol2 = if pole2 == '-' then negColCount !! col2 + 1 else negColCount !! col2
            -- calculate new negative pole counts in rows and columns

            row1PosValid = left !! row1 == -1 || testPosRow1 <= left !! row1  -- check row1 positive constraint
            row2PosValid = left !! row2 == -1 || testPosRow2 <= left !! row2  -- check row2 positive constraint
            col1PosValid = top !! col1 == -1 || testPosCol1 <= top !! col1  -- check col1 positive constraint
            col2PosValid = top !! col2 == -1 || testPosCol2 <= top !! col2  -- check col2 positive constraint
            -- check if positive pole constraints would be violated
                    
            row1NegValid = right !! row1 == -1 || testNegRow1 <= right !! row1  -- check row1 negative constraint
            row2NegValid = right !! row2 == -1 || testNegRow2 <= right !! row2  -- check row2 negative constraint
            col1NegValid = bottom !! col1 == -1 || testNegCol1 <= bottom !! col1  -- check col1 negative constraint
            col2NegValid = bottom !! col2 == -1 || testNegCol2 <= bottom !! col2  -- check col2 negative constraint
            -- check if negative pole constraints would be violated
                    
            maxAllocValid = testPosRow1 <= posRowMax !! row1 && testPosRow2 <= posRowMax !! row2 &&
                            testPosCol1 <= posColMax !! col1 && testPosCol2 <= posColMax !! col2 &&
                            testNegRow1 <= negRowMax !! row1 && testNegRow2 <= negRowMax !! row2 &&
                            testNegCol1 <= negColMax !! col1 && testNegCol2 <= negColMax !! col2
            -- check if the new counts are within the maximum allowed counts

            constraintsValid = row1PosValid && row2PosValid && col1PosValid && col2PosValid &&
                              row1NegValid && row2NegValid && col1NegValid && col2NegValid && maxAllocValid
            -- check if all constraints are satisfied
          in
            if not constraintsValid
            then False
            else  -- check for adjacent like poles
              let
                notAdjacent1 = noAdjacentLikePoles solution row1 col1 pole1 height width
                notAdjacent2 = noAdjacentLikePoles solution row2 col2 pole2 height width
              in
                notAdjacent1 && notAdjacent2  -- both poles must not have adjacent like poles

noAdjacentLikePoles :: [[Char]] -> Int -> Int -> Char -> Int -> Int -> Bool
noAdjacentLikePoles solution row col pole height width =
-- function noAdjacentLikePoles to check if there are adjacent like poles
-- solution: current state of the board
-- row, col: position of the pole
-- pole: polarity of the pole
-- height, width: dimensions of the board
-- the function returns True if there are no adjacent like poles, False otherwise

  let  -- check if the pole is at the edge of the board
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

    checkDirection (dr, dc) =  
    -- check each adjacent cell for a matching pole
      let r = row + dr  -- calculate adjacent row
          c = col + dc  -- calculate adjacent column
      in
        if r < 0 || r >= height || c < 0 || c >= width
        then True  
        else solution !! r !! c /= pole  -- should not have same pole in adjacent cell
  in
    all checkDirection directions  -- all adjacent positions must not have the same pole

constraintsSatisfied :: [[Char]] -> (Int, Int, [Int], [Int], [Int], [Int]) -> [Int] -> [Int] -> [Int] -> [Int] -> Bool
constraintsSatisfied solution (height, width, left, right, top, bottom) posRowCount negRowCount posColCount negColCount =
-- function constraintsSatisfied to check if all constraints are satisfied
-- solution: current state of the board
-- boardInfo: tuple of board dimensions and constraints
-- posRowCount, negRowCount, posColCount, negColCount: current counts of poles
-- the function returns True if all constraints are satisfied, False otherwise

  let
    areRowsValid = all (\row ->
      (left !! row == -1 || posRowCount !! row == left !! row) &&  -- check positive row constraint 
      (right !! row == -1 || negRowCount !! row == right !! row)  -- check negative row constraint 
      ) [0..height-1]
    -- check that row constraints are satisfied

    areColsValid = all (\col ->
      (top !! col == -1 || posColCount !! col == top !! col) &&  -- check positive column constraint
      (bottom !! col == -1 || negColCount !! col == bottom !! col)  -- check negative column constraint
      ) [0..width-1]
    -- check that column constraints are satisfied
  in
    areRowsValid && areColsValid  -- all constraints must be satisfied