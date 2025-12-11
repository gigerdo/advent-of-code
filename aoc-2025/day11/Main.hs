module Main where

import Control.Monad.State.Lazy
import Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.Attoparsec.Text as A
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace

main :: IO ()
main = do
  -- contents <- TIO.readFile "./day11/sample.txt"
  contents <- TIO.readFile "./day11/input.txt"
  let input = T.lines contents
  let parsedItems = rights $ map (parseOnly lineParser) input
  let result = countPaths parsedItems
  putStrLn ("part01=" ++ show result)

  let result2 = countSvrPaths parsedItems
  putStrLn ("part02=" ++ show result2)

countSvrPaths :: [ParsedLineData] -> Int
countSvrPaths input = do
  let deviceMap = Map.insert (T.pack "out") [] $ Map.fromList $ map (\x -> (device x, outputs x)) input

  let a = evalState (countSvrPathsRec deviceMap (T.pack "dac") (T.pack "svr")) Map.empty
  let b = evalState (countSvrPathsRec deviceMap (T.pack "fft") (T.pack "dac")) Map.empty
  let c = evalState (countSvrPathsRec deviceMap (T.pack "out") (T.pack "fft")) Map.empty

  let d = evalState (countSvrPathsRec deviceMap (T.pack "fft") (T.pack "svr")) Map.empty
  let e = evalState (countSvrPathsRec deviceMap (T.pack "dac") (T.pack "fft")) Map.empty
  let f = evalState (countSvrPathsRec deviceMap (T.pack "out") (T.pack "dac")) Map.empty

  (a * b * c) + (d * e * f)

countSvrPathsRec :: Map Text [Text] -> Text -> Text -> State (Map Text Int) Int
countSvrPathsRec deviceMap target position
  | position == target = return 1
  | otherwise = do
      m <- gets $ Map.lookup position
      case m of
        Just res -> return res
        Nothing -> do
          let outputs = deviceMap Map.! position
          paths <- mapM (countSvrPathsRec deviceMap target) outputs
          let result = sum paths
          modify (Map.insert position result)
          -- traceShow (position, path) (return result)
          return result

countPaths :: [ParsedLineData] -> Int
countPaths input = do
  let deviceMap = Map.fromList $ map (\x -> (device x, outputs x)) input
  countPathsRec deviceMap (T.pack "you")

countPathsRec :: Map Text [Text] -> Text -> Int
countPathsRec deviceMap position
  | position == T.pack "out" = 1
  | otherwise = do
      let outputs = deviceMap Map.! position
      let paths = map (countPathsRec deviceMap) outputs
      sum paths

-- | A data structure to hold the results of our complex line parse
data ParsedLineData = ParsedLineData
  { device :: Text,
    outputs :: [Text]
  }
  deriving (Show)

-- | The main parser for the entire line structure
lineParser :: Parser ParsedLineData
lineParser = do
  deviceVal <- A.takeWhile (/= ':')
  _ <- A.string (T.pack ": ")
  outputsVal <- A.takeWhile1 (/= ' ') `A.sepBy` A.skipSpace
  A.endOfInput
  return $ ParsedLineData deviceVal outputsVal
