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
  -- contents <- readFile "./day09/sample.txt"
  contents <- readFile "./day09/input.txt"
  let input = map toTile $ map (\x -> map (\y -> read y :: Int) (splitOn "," x)) $ lines contents
  let pairs = buildPairs input
  let rectangles = sortBy (comparing (\(_, _, dist) -> -dist)) $ map calculateDistance pairs

  let result = head rectangles
  putStrLn ("part01=" ++ show result)

  let edges = zip input (tail input) ++ [(last input, head input)]
  let result2 = head $ filter (isInsidePolygon edges) rectangles
  putStrLn ("part02=" ++ show result2)

type Tile = (Int, Int)

toTile :: [Int] -> Tile
toTile [x, y] = (x, y)

isInsidePolygon :: [(Tile, Tile)] -> (Tile, Tile, Int) -> Bool
isInsidePolygon edges ((ax, ay), (bx, by), _) = do
  let (minX, minY) = (min ax bx, min ay by)
  let (maxX, maxY) = (max ax bx, max ay by)
  let result = all (not . overlaps ((minX, minY), (maxX, maxY))) edges
  traceShow ("Rectangle", (minX, minY), (maxX, maxY), (maxX - minX + 1) * (maxY - minY + 1), result) (result)

overlaps :: (Tile, Tile) -> (Tile, Tile) -> Bool
overlaps ((rax, ray), (rbx, rby)) ((eax, eay), (ebx, eby)) = do
  let minX = min eax ebx
  let maxX = max eax ebx
  let minY = min eay eby
  let maxY = max eay eby
  let horizontalOverlap = minY == maxY && minY > ray && maxY < rby
  let verticalOverlap = minX == maxX && minX > rax && minX < rbx
  let result
        | horizontalOverlap && minX < rax && maxX > rax = True
        | horizontalOverlap && minX < rbx && maxX > rbx = True
        | horizontalOverlap && minX > rax && maxX < rbx = True
        | verticalOverlap && minY < ray && maxY > ray = True
        | verticalOverlap && minY < rby && maxY > rby = True
        | verticalOverlap && minY > ray && maxY < rby = True
        | otherwise = False
  -- traceShow ("checkrect", result, horizontalOverlap, verticalOverlap, (minX, minY), (maxX, maxY)) (result)
  result

buildPairs :: [Tile] -> [(Tile, Tile)]
buildPairs [] = []
buildPairs (first : remaining) = do
  let pairs = map (\x -> (first, x)) remaining
  pairs ++ buildPairs remaining

calculateDistance :: (Tile, Tile) -> (Tile, Tile, Int)
calculateDistance (a, b) = do
  let (ax, ay) = a
  let (bx, by) = b
  let distance = (abs (bx - ax) + 1) * (abs (by - ay) + 1)
  (a, b, distance)
