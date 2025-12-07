module Main where

import Control.Monad.State.Lazy
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord (comparing)
import Debug.Trace

main :: IO ()
main = do
  -- contents <- readFile "./day07/sample.txt"
  contents <- readFile "./day07/input.txt"
  let input = lines contents
  let start = fromJust $ elemIndex 'S' (input !! 0)

  let result = shootTachyon input start 0 []
  putStrLn ("part01=" ++ show (length result))

  let result2 = evalState (shootQuantumTachyon input start 0) Map.empty
  putStrLn ("part02=" ++ show result2)

shootQuantumTachyon :: [[Char]] -> Int -> Int -> State (Map (Int, Int) Int) Int
shootQuantumTachyon input x _ | x < 0 || x >= length (input !! 0) = return 0
shootQuantumTachyon input x y | y >= length input = return 1
shootQuantumTachyon input x y = do
  let currentVal = (input !! y) !! x
  if currentVal == '^'
    then do
      m <- gets $ Map.lookup (x, y)
      case m of
        Just res -> return res
        Nothing -> do
          first <- shootQuantumTachyon input (x - 1) y
          second <- shootQuantumTachyon input (x + 1) y
          let result = first + second
          modify (Map.insert (x, y) result)
          return result
    else
      shootQuantumTachyon input x (y + 1)

shootTachyon :: [[Char]] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
shootTachyon input x _ _ | x < 0 || x >= length (input !! 0) = []
shootTachyon input x y splits | y >= length input = nub splits
shootTachyon input x y splits = do
  let currentVal = (input !! y) !! x
  let next =
        if currentVal == '^' && elem (x, y) splits
          then splits
          else
            if currentVal == '^'
              then do
                let newSplits = splits ++ [(x, y)]
                let first = shootTachyon input (x - 1) y newSplits
                let second = shootTachyon input (x + 1) y first
                nub (first ++ second)
              else shootTachyon input x (y + 1) splits
  -- traceShow (x, y, currentVal) (nub next)
  nub next