
main :: IO ()
main = do
    contents <- readFile "input.txt"
    -- part 1
    print $ length $ filter ((==0) . (`mod` 100)) $ scanl rotate 50 (lines contents)
    -- part 2
    print $ sum $ map (uncurry numsbetween) $ (\l -> zip l (tail l)) $ scanl rotate 50 (lines contents)

rotate :: Int -> String -> Int
rotate k (c:n) = case c of
    'R' -> k + read n
    'L' -> k - read n

numsbetween :: Int -> Int -> Int
numsbetween a b = if b > a then length $ takeWhile (>a) $ iterate (subtract 100) $ roundDown b else length $ takeWhile (<a) $ iterate (+100) $ roundUp b
    where
        roundDown n = n - (n `mod` 100)
        roundUp n = if n `mod` 100 == 0 then n else n + ((-n) `mod` 100)