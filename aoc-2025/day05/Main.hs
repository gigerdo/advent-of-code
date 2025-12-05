module Main where

import Data.List (sortBy)
import Data.List.Split
import Data.Ord (comparing)
import Debug.Trace

main :: IO ()
main = do
  -- contents <- readFile "./day05/sample.txt"
  contents <- readFile "./day05/input.txt"
  let (r : (f : _)) = splitOn "\n\n" contents
  let ranges = map (\x -> map (\y -> read y :: Int) $ splitOn "-" x) $ lines r
  let fruits = map (\x -> read x :: Int) $ lines f
  let result = filter (isFresh ranges) fruits
  putStrLn ("part01=" ++ show (length result))

  let combinedRanges = combineRanges (sortBy (comparing head) ranges) []
  let result2 = sum $ map (\x -> (x !! 1) - (x !! 0) + 1) combinedRanges
  putStrLn ("part02=" ++ show result2)

combineRanges :: [[Int]] -> [[Int]] -> [[Int]]
combineRanges [x] processed = processed ++ [x]
combineRanges ranges processed = do
  let (first:(second:_)) = ranges
  let combined =
        if first !! 1 >= second !! 0 && first !! 1 <= second !! 1
          then combineRanges ([[first !! 0, second !! 1]] ++ (drop 2 ranges)) processed
          else
            if first !! 1 >= second !! 1
              then combineRanges ([[first !! 0, first !! 1]] ++ (drop 2 ranges)) processed
              else combineRanges (drop 1 ranges) (processed ++ [first])
  -- traceShow (ranges) (combined)
  combined

isFresh :: [[Int]] -> Int -> Bool
isFresh ranges x = do
  let fresh = filter (inRange x) ranges
  (length fresh) > 0

inRange :: Int -> [Int] -> Bool
inRange x range = do
  let start = range !! 0
  let end = range !! 1
  x >= start && x <= end