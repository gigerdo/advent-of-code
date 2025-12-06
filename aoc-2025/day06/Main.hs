module Main where

import Data.Char (isSpace)
import Data.List
import Data.List.Split
import Data.Ord (comparing)
import Data.Text (strip)
import Debug.Trace

main :: IO ()
main = do
  -- contents <- readFile "./day06/sample.txt"
  contents <- readFile "./day06/input.txt"

  let input = transpose $ map words $ lines contents
  let result = sum $ map part01 input
  putStrLn ("part01=" ++ show result)

  let l = lines contents
  let spacing = calculateSpacing (drop 1 $ l !! (length l - 1)) 0 1 []
  let result2 = sum $ iterateSpacings l spacing
  putStrLn ("part02=" ++ show result2)

iterateSpacings :: [String] -> [(Int, Int)] -> [Int]
iterateSpacings lines [] = []
iterateSpacings lines ((start, end) : remainingSpacings) = do
  let nLines = length lines
  let values = map (\x -> take (end - start) $ drop start x) lines
  let numbers = map (\x -> read x :: Int) $ filter (not . all isSpace) $ transpose $ take (nLines - 1) values
  let operation = (drop (nLines - 1) values) !! 0 !! 0
  let result = calculate [operation] numbers
  let next = iterateSpacings lines remainingSpacings
  -- traceShow (numbers, operation, result) ([result] ++ next)
  [result] ++ next

calculateSpacing :: String -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
calculateSpacing "" previous current collected = collected ++ [(previous, current)]
calculateSpacing (next : remaining) previous current collected = do
  if next == ' '
    then calculateSpacing remaining previous (current + 1) collected
    else calculateSpacing remaining current (current + 1) (collected ++ [(previous, current)])

part01 :: [String] -> Int
part01 input = do
  let (operator : values) = reverse input
  let numbers = map (\x -> read x :: Int) values
  let result = calculate operator numbers
  traceShow (operator, numbers, result) (result)

calculate :: String -> [Int] -> Int
calculate "+" numbers = sum numbers
calculate "*" numbers = product numbers
calculate other numbers = traceShow ("unknown-operator", other, numbers) 0