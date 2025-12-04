module Main where

import Debug.Trace
import Control.Lens

main :: IO ()
main = do
  -- contents <- readFile "./day04/sample.txt"
  contents <- readFile "./day04/input.txt"
  let fileLines = lines contents
      result = findAccessible fileLines
  putStrLn ("part01=" ++ show (length result))
  
  let result2 = findAccessibleRec fileLines
  putStrLn ("part02=" ++ show result2)

findAccessibleRec :: [String] -> Int
findAccessibleRec grid = do
  let removedPapers = findAccessible grid
  let removedPaperCount = length removedPapers
  let newGrid = removePapers grid removedPapers
  let nestedCount = if removedPaperCount > 0 then findAccessibleRec newGrid else 0
  removedPaperCount + nestedCount

removePapers :: [String] -> [(Int, Int)] -> [String]
removePapers grid [] = grid
removePapers grid ((x, y):remainingCoords) = do
  let newRow = (grid !! y) & element x .~ '.'
  let newGrid = grid & element y .~ newRow
  removePapers newGrid remainingCoords


findAccessible :: [String] -> [(Int, Int)]
findAccessible grid = do
  let height = length grid
  let width = length (grid !! 0)
  let coordinates = [ (x,y) | x <- [0..width-1], y <- [0..height-1]]
  let result = filter (isAccessible grid) coordinates
  result

isAccessible :: [String] -> (Int, Int) -> Bool
isAccessible grid (x, y) = do
  let surroundingCoords = [ (xn,yn) | xn <- [x-1..x+1], yn <- [y-1..y+1], xn /= x || yn /= y]
  let papers = filter (hasPaper grid) surroundingCoords
  let accessible = ((grid !! y) !! x) == '@' && length papers < 4
  -- traceShow(x, y, accessible, surroundingCoords, papers) (accessible)
  accessible

hasPaper :: [String] -> (Int, Int) -> Bool
hasPaper grid (x, y) = do
  let height = length grid
  let width = length (grid !! 0)
  if x >= 0 && x < width && y >= 0 && y < height
    then ((grid !! y) !! x) == '@'
    else False