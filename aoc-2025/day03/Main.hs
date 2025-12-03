module Main where

import Data.List.Split
import Debug.Trace
import Data.Text (take, drop, pack, unpack)
import Data.Char (digitToInt)
import Data.Ord
import Data.List (sortBy, take, drop, elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  -- contents <- readFile "./day03/sample.txt"
  contents <- readFile "./day03/input.txt"
  let fileLines = lines contents
      result = sum $ map (\x -> (findJoltage x (-1) (-1))) fileLines
  putStrLn ("part01=" ++ show result)
  
  let result2 = sum $ map (\x -> (findHighJoltage x)) fileLines
  putStrLn ("part02=" ++ show result2)


findJoltage :: String -> Int -> Int -> Int
findJoltage "" highest secondHighest = 10 * highest + secondHighest
findJoltage battery highest secondHighest = do
  let nextChar = head battery
      value = digitToInt nextChar
      remaining = tail battery
      (newHighest, newSecondHighest) =
        if value > highest && remaining == ""
          then (highest, value)
          else
         if value > highest
        then (value, (-1))
        else if value > secondHighest
          then (highest, value)
          else (highest, secondHighest)

      next = findJoltage remaining newHighest newSecondHighest
  -- traceShow (battery, highest, secondHighest, value, remaining) (next)
  next


findHighJoltage :: String -> Int
findHighJoltage bank = do
  let l = map digitToInt bank
  let sorted = Data.List.take 12 (sortBy (comparing Down) l)
  let result = descend l []
  let number = foldl1 (\x y -> 10*x+y) result
  -- traceShow (l, sorted, result, number) (number)
  number

descend :: [Int] -> [Int]-> [Int]
descend _ solution | length solution == 12 = solution
descend remainingNumbers solution = do
  let sortedRemaining = sortBy (comparing Down) remainingNumbers
  let spaceNeeded = 12 - (length solution)
  let maxIndex = (length remainingNumbers) - spaceNeeded
  let toUse = head $ sortBy (comparing Down) $ filter (\x -> indexIsBelow x remainingNumbers maxIndex) sortedRemaining
  let toUseIndex = fromJust $ elemIndex toUse remainingNumbers
  let result = descend (Data.List.drop (toUseIndex + 1) remainingNumbers) (solution ++ [toUse])
  -- traceShow (remainingNumbers, spaceNeeded, maxIndex, toUse) (result)
  result

indexIsBelow :: Int -> [Int] -> Int -> Bool
indexIsBelow searchFor numbers maxIndex = do
  let index = fromJust $ elemIndex searchFor numbers
  if index <= maxIndex then True
  else False