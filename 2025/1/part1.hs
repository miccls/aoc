
-- One-liner below hehe
-- main :: IO ()
-- main = readFile "input.txt" >>= print . length . filter (==0) . scanl (\c x -> (c + x) `mod` 100) 50 . map (\s -> if head s == 'L' then read (tail s) else - read (tail s)) . lines


parseInstruction :: String -> Integer
parseInstruction ('L':xs) = read xs
parseInstruction ('R':xs) = - read xs
parseInstruction _        = error "Invalid format"

compoundRotation :: Integer -> Integer -> Integer
compoundRotation carry x = (carry + x) `mod` 100

rotations :: [Integer] -> [Integer]
rotations = scanl compoundRotation 50

countZeros :: [Integer] -> Int
countZeros = length . filter (==0)

instructions :: IO [Integer]
instructions = map parseInstruction . lines <$> readFile "input.txt"

main :: IO ()
main = print =<< (countZeros . rotations <$> instructions)