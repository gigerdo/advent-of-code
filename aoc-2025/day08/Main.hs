module Main where

import Control.Monad.State.Lazy
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Debug.Trace

main :: IO ()
main = do
  -- contents <- readFile "./day08/sample.txt"
  contents <- readFile "./day08/input.txt"
  let input = map toBox $ map (\x -> map (\y -> read y :: Int) (splitOn "," x)) $ lines contents
  let pairs = buildPairs input
  let distances = sortBy (comparing (\(_, _, dist) -> dist)) $ map calculateDistance pairs

  let result = connectBoxes distances
  putStrLn ("part01=" ++ show result)

  let result2 = findOneCircuit input distances
  putStrLn ("part02=" ++ show result2)

findOneCircuit :: [Box] -> [(Box, Box, Int)] -> Int
findOneCircuit input distances = do
  let startingCircuits = map (\x -> [x]) input
  let ((ax, _, _), (bx, _, _)) = findLastConnectingBox distances startingCircuits
  ax * bx

findLastConnectingBox :: [(Box, Box, Int)] -> [[Box]] -> (Box, Box)
findLastConnectingBox (shortest : remainingPairs) circuits = do
  let (a, b, _) = shortest
  let (aAndB, remaining) = partition (\x -> elem a x || elem b x) circuits
  let combined = nub $ concat aAndB :: [Box]
  let newCircuits = remaining ++ [combined]
  if (length newCircuits) == 1
    then (a, b)
    else findLastConnectingBox remainingPairs newCircuits

type Box = (Int, Int, Int)

toBox :: [Int] -> Box
toBox [x, y, z] = (x, y, z)

connectBoxes :: [(Box, Box, Int)] -> Int
connectBoxes distances = do
  let circuits = connectShortestBoxes (take 1000 distances) []
  let largestCircuits = take 3 $ sortBy (comparing Down) $ map (length) circuits
  traceShow (largestCircuits) (product largestCircuits)

buildPairs :: [Box] -> [(Box, Box)]
buildPairs [] = []
buildPairs (first : remaining) = do
  let pairs = map (\x -> (first, x)) remaining
  pairs ++ buildPairs remaining

connectShortestBoxes :: [(Box, Box, Int)] -> [[Box]] -> [[Box]]
connectShortestBoxes [] circuits = circuits
connectShortestBoxes (shortest : remainingPairs) circuits = do
  let (a, b, _) = shortest
  let (aCircuit, aRemaining) = partition (\x -> elem a x) circuits
  let (bCircuit, bRemaining) = partition (\x -> elem b x) circuits
  let aInCircuit = (length aCircuit) > 0
  let bInCircuit = (length bCircuit) > 0
  let newCircuits =
        if not aInCircuit && not bInCircuit
          then circuits ++ [[a, b]]
          else
            if aInCircuit && not bInCircuit
              then aRemaining ++ [(head aCircuit) ++ [b]]
              else
                if not aInCircuit && bInCircuit
                  then bRemaining ++ [(head bCircuit) ++ [a]]
                  else do
                    let (aAndB, remaining) = partition (\x -> elem a x || elem b x) circuits
                    let combined = nub $ concat aAndB :: [Box]
                    remaining ++ [combined]
  connectShortestBoxes remainingPairs newCircuits

calculateDistance :: (Box, Box) -> (Box, Box, Int)
calculateDistance (a, b) = do
  let (ax, ay, az) = a
  let (bx, by, bz) = b
  let distance = ((square (bx - ax)) + (square (by - ay)) + (square (bz - az)))
  (a, b, distance)

square :: Int -> Int
square a = a * a