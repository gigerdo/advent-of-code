module Main where

import Debug.Trace

main :: IO ()
main = do
  -- contents <- readFile "./day01/sample.txt"
  contents <- readFile "./day01/input.txt"
  let fileLines = lines contents
  let (dialPos, zeroes) = foldl updateDial (50, 0) fileLines
  putStrLn ("part01=" ++ show zeroes ++ ", dialpos=" ++ show dialPos)

  let (dialPos2, zeroes2) = foldl updateDialPart2 (50, 0) fileLines
  putStrLn ("part02=" ++ show zeroes2 ++ ", dialpos=" ++ show dialPos2)

updateDial :: (Int, Int) -> String -> (Int, Int)
updateDial acc [] = acc
updateDial (currentPos, zeroes) (firstChar : rest) = do
  let num = read rest :: Int
      newPos =
        if firstChar == 'L'
          then (currentPos - num) `mod` 100
          else (currentPos + num) `mod` 100
  if newPos == 0
    then (newPos, zeroes + 1)
    else (newPos, zeroes)

updateDialPart2 :: (Int, Int) -> String -> (Int, Int)
updateDialPart2 acc [] = acc
updateDialPart2 (currentPos, currentZeros) (firstChar : rest) = do
  let num = read rest :: Int
      newPos =
        if firstChar == 'L'
          then (currentPos - num)
          else (currentPos + num)
      newPos2 = newPos `mod` 100
      zerosPassed = div newPos 100
      addedZeros
        | newPos >= 100 = zerosPassed
        | newPos <= 0 && currentPos == 0 = (abs zerosPassed) - 1 -- Go left when starting on 0
        | newPos <= 0 && newPos2 == 0 = (abs zerosPassed) + 1 -- Going left and ending on 0
        | newPos <= 0 = abs zerosPassed
        | otherwise = 0
  traceShow (firstChar, num, currentPos, newPos, newPos2, zerosPassed) (newPos2, currentZeros + addedZeros)