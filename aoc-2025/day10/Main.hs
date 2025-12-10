module Main where

import Control.Lens
import Control.Monad (forM, forM_)
import Control.Monad.State.Lazy
import Data.Attoparsec.Text (Parser, parseOnly, (<?>))
import qualified Data.Attoparsec.Text as A
import Data.Either (fromRight, rights)
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Data.SBV
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace

main :: IO ()
main = do
  -- contents <- TIO.readFile "./day10/sample.txt"
  contents <- TIO.readFile "./day10/input.txt"
  let input = T.lines contents
  let parsedItems = rights $ map (parseOnly lineParser) input
  let result = sum $ map calculateFewestButtonPresses parsedItems
  putStrLn ("part01=" ++ show result)

  result2 <- mapM solveSmallestPositiveSolution parsedItems
  putStrLn ("part02=" ++ show (sum result2))

-- | Solves the system x + y + z = 10, minimizes x, subject to x, y, z >= 1.
solveSmallestPositiveSolution :: ParsedLineData -> IO Integer
solveSmallestPositiveSolution input = do
  result <- optimize Lexicographic $ do
    let targetJoltage = map fromIntegral (joltage input) :: [SInteger]
    let btns = (buttons input)
    vars <- forM [0 .. (length btns) - 1] (\i -> sInteger ("v" ++ show i))

    -- Every variable must be >= 0
    forM_ vars $ \v ->
      constrain $ v Data.SBV..>= 0

    -- Add matrix constraints
    let zeroes = take (length targetJoltage) (repeat (take (length btns) (repeat 0)))
    let matrix = foldl (\acc x -> setButtonInMatrix acc x (btns !! x)) zeroes [0 .. (length btns) - 1]
    let results = matrix `mul` vars
    forM_ (zip results targetJoltage) $ \(row, target) ->
      constrain $ row .== target

    -- Minimize button presses -> min of the sum of variables
    minimize "min_sum" (sum vars)

  case result of
    LexicographicResult smtResult -> do
      let minSumValue = getModelValue "min_sum" smtResult
      case minSumValue of
        Just n -> return n
        Nothing -> return 0

mul :: (Num a) => [[a]] -> [a] -> [a]
mul matrix vector = map (\row -> dotProduct row vector) matrix

dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct xs ys = sum (zipWith (*) xs ys)

setButtonInMatrix :: [[SInteger]] -> Int -> [Int] -> [[SInteger]]
setButtonInMatrix matrix index button = do
  foldl (\matrix idx -> matrix & element idx %~ (\x -> x & element index .~ 1)) matrix button

calculateFewestButtonPresses :: ParsedLineData -> Int
calculateFewestButtonPresses input = do
  let startingState = map (\x -> '.') (T.unpack (lights input))
  let result = evalState (checkNextButtonPress (T.unpack (lights input)) (buttons input)) [(startingState, 0, [])]
  traceShow (input, result) (result)

checkNextButtonPress :: String -> [[Int]] -> State ([(String, Int, [Int])]) Int
checkNextButtonPress lights buttons = do
  queue <- get
  let (state, steps, lastButton) = head queue
  modify (drop 1)
  if (state == lights)
    then return steps
    else do
      let b = filter (not . (== lastButton)) buttons
      let newStates = map (\x -> (applyButton state x, steps + 1, x)) b
      let existingStates = map (\(state, _, _) -> state) queue
      let filteredStates = filter (\(x, _, _) -> not (elem x existingStates)) newStates
      modify (++ filteredStates)
      -- traceShow (filteredStates) (checkNextButtonPress lights buttons)
      checkNextButtonPress lights buttons

applyButton :: String -> [Int] -> String
applyButton state button = do
  let newState = foldl flipLight state button
  newState

flipLight :: String -> Int -> String
flipLight state index = do
  let char = state !! index
  if char == '#'
    then state & element index .~ '.'
    else state & element index .~ '#'

-- | A data structure to hold the results of our complex line parse
data ParsedLineData = ParsedLineData
  { lights :: Text,
    buttons :: [[Int]],
    joltage :: [Int]
  }
  deriving (Show)

-- | The main parser for the entire line structure
lineParser :: Parser ParsedLineData
lineParser = do
  lightsVal <- lightsParser
  A.skipSpace
  tuplesVal <- A.many' (A.skipSpace *> tupleParser)
  A.skipSpace
  setVal <- setParser
  A.endOfInput
  return $ ParsedLineData lightsVal tuplesVal setVal

-- | Parses the initial [##.##] section
lightsParser :: Parser Text
lightsParser = do
  A.char '['
  val <- A.takeWhile (/= ']') <?> "lights"
  A.char ']'
  return val

-- | Parses a single parenthesized tuple like (0,2,3)
tupleParser :: Parser [Int]
tupleParser = do
  A.char '('
  -- SepBy parses one or more things separated by a delimiter (the comma)
  vals <- A.sepBy intParser (A.char ',')
  A.char ')'
  return vals <?> "tuple group"

-- | Parses the final curly-braced set like {34,18,...}
setParser :: Parser [Int]
setParser = do
  A.char '{'
  vals <- A.sepBy intParser (A.char ',')
  A.char '}'
  return vals <?> "final set"

-- | Helper parser for a single integer (used inside tuples and sets)
intParser :: Parser Int
intParser = A.decimal <?> "integer"