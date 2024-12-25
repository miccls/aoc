import Data.List.Split (splitOn)
import GHC.Float (Floating (logBase))
import System.Directory (canonicalizePath)
import System.IO

(><) :: Int -> Int -> Int
x >< y = read (show x ++ show y) :: Int

-- Write a function which processes each line
isReproducible :: (Int, Int, [Int]) -> Bool
isReproducible (lhs, carry, []) = lhs == carry
isReproducible (lhs, 0, y : ys) = isReproducible (lhs, y, ys)
isReproducible (lhs, carry, y : ys) = any (\op -> isReproducible (lhs, op carry y, ys)) [(+), (*), (><)]

-- Make all lines into pair of int and sequence of ints
parseToInts :: String -> (Int, Int, [Int])
parseToInts str =
  let [firstPart, secondPart] = splitOn ": " str
      -- Convert the first part to an Int
      firstInt = read firstPart :: Int
      -- Convert the second part into a list of integers by splitting by spaces
      secondList = map read (words secondPart) :: [Int]
   in (firstInt, 0, secondList)

main :: IO ()
main = do
  -- Read the file contents and split into lines
  fullPath <- canonicalizePath "input.txt"
  input <- lines <$> readFile fullPath
  -- Print each line after processing
  let parsed_input = map parseToInts input
  print (foldl (\acc (lhs, x, y) -> if isReproducible (lhs, x, y) then acc + lhs else acc) 0 parsed_input)