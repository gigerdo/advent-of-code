module Main where

import Data.List.Split
import Debug.Trace
import Data.Text (take, drop, pack, unpack)

main :: IO ()
main = do
  -- contents <- readFile "./day02/sample.txt"
  contents <- readFile "./day02/input.txt"
  let ranges = map (\x -> (read (x !! 0) :: Int, read (x !! 1) :: Int)) (map (\x -> splitOn "-" x) (splitOn "," contents))
      invalidIds = filter isInvalid (concat (map expandRange ranges))
      invalidIdsP2 = filter isInvalidP2 (concat (map expandRange ranges))
      result = sum invalidIds
      resultP2 = sum invalidIdsP2
  putStrLn ("part01=" ++ show result)
  putStrLn ("part02=" ++ show resultP2)

expandRange :: (Int, Int) -> [Int]
expandRange (start, end) = [start .. end]

isInvalid :: Int -> Bool
isInvalid id = do
  let idStr = show id 
      idLength = length idStr
      half = idLength `div` 2
      firstHalf = Data.Text.take half (pack idStr)
      secondHalf = Data.Text.drop half (pack idStr)
      result =
        if firstHalf == secondHalf
          then True
          else
            False
  -- traceShow (idLength, idStr, firstHalf, secondHalf, result) (result)
  result


isInvalidP2 :: Int -> Bool
isInvalidP2 id = do
  let idStr = show id 
      idLength = length idStr
      half = idLength `div` 2
      r = [1..half]
      result =
        if any (\x -> matches (unpack (Data.Text.take x (pack idStr))) idStr) r
          then True
          else
            False
  -- traceShow ("overall", idStr, result) (result)
  result

matches :: String -> String -> Bool
matches part [] = True
matches part rem = do
  let ln = length part
      x = (unpack (Data.Text.take ln (pack rem)))
      result = if part == x
        then matches part (unpack (Data.Text.drop ln (pack rem)))
      else
        False
  -- traceShow (rem, part, ln, x, result) (result)
  result