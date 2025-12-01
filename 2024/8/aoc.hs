import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Map.Strict qualified as Map

type Position = (Int, Int)

type Vector = (Double, Double)

-- Get all frequencies and their positions
getFrequenciesAndPositions :: [[Char]] -> Map.Map Char [Position]
getFrequenciesAndPositions grid =
  foldr
    (\((r, c), ch) acc -> if ch == '.' then acc else Map.insertWith (++) ch [(r, c)] acc)
    Map.empty
    indexedChars
  where
    indexedChars = [((r, c), ch) | (r, row) <- zip [0 ..] grid, (c, ch) <- zip [0 ..] row]

getGrid :: [[Char]] -> [Position]
getGrid grid = [(r, c) | r <- [0 .. length grid - 1], c <- [0 .. length (head grid) - 1]]

(><) :: Int -> Position -> Position
a >< (p1, p2) = (a * p1, a * p2)

-- Check if there is an antinode there by computing the distance to all antennas and then doubling them to see if there is any match
antiNode1 :: Position -> [Position] -> Bool
antiNode1 (p1, p2) antennas =
  let distances = map (\(x1, x2) -> (x1 - p1, x2 - p2)) antennas
   in any (\x -> (x /= (0, 0)) && ((2 >< x) `elem` distances)) distances

magnitude :: (Floating a) => [a] -> a
magnitude v = sqrt (sum (map (^ 2) v))

normalize :: Position -> Vector
normalize (x, y)
  | x == 0 && y == 0 = (0, 0)
  | otherwise =
      let magSq = sqrt $ fromIntegral (x * x + y * y) -- Avoid sqrt
       in (fromIntegral x / magSq, fromIntegral y / magSq)

dotProduct :: Vector -> Vector -> Double
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

nearZero :: Double -> Bool
nearZero x = abs x < 1e-12

antiNode2 :: Position -> [Position] -> Bool
antiNode2 p [antenna] = False
antiNode2 (p1, p2) antennas =
  let distances = map (\(x1, x2) -> normalize (x1 - p1, x2 - p2)) antennas
   in any (\x -> (x == (0, 0)) || (length (filter (\y -> nearZero (1 - abs (dotProduct x y))) distances) > 1)) distances -- If in same direction then in line

checkAntinodesForFrequencies :: [(Char, [Position])] -> Position -> Bool
checkAntinodesForFrequencies freqList p =
  any (\(_, positions) -> antiNode2 p positions) freqList

-- Go through all positions and check if
main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ length $ filter (checkAntinodesForFrequencies $ Map.toList (getFrequenciesAndPositions input)) (getGrid input)