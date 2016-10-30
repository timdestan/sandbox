import Data.Function (on)

-- I know this exists, but reimplementing for the sake of exercise.
-- import Data.List (subsequences)

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (h : t) = map ((:) h) tailSubs ++ tailSubs
  where tailSubs = subsequences t

closest :: [Int] -> [Int] -> Int
closest a b =
  minimum $ map sumOfDiffsWithA $ filter sameLengthAsA $ subsequences b
  where
    sumOfDiffsWithA b' = sum $ map diff (zip a b')
    sameLengthAsA = ((==) `on` length) a
    diff (x, y) = abs (x - y)

main :: IO ()
main = do
  checkEq 2 (closest [1, 2, 6] [0, 1, 3, 4, 5])
  checkEq 7 (closest [1, 2, 1, 2, 1, 2]
                     [3, 0, 0, 3, 0, 3, 3, 0, 0])
  checkEq 0 (closest [1, 1, 1, 1, 1, 1]
                     [1, 1, 1, 1, 1, 1, 1, 1, 1])
  checkEq 270 (closest [13, 5, 3, -1, -9, 20, 5, -17, 20, -11, -6, 1, 17, 18,
                        20, -6, 11, 12, 3, -8]
                       [1, 1, -18, -3, -9, 16, 5, 13, -2, 4, -9, -16, -20, 13,
                        -3, 10, 20, -5, -20, 2])
  -- checkEq 1928 (closest [-26, -35, 44, 23, 7, -40, -14, 18, 39, -12, -22, -5, 4,
  --                        10, 0, -11, 45, -16, 2, 46, -45, 2, -3, -50, -17, 49,
  --                        47, -15, 49, -15, 16, 43, 33, 22, -34, 48, -41, 12, 19,
  --                        -17, 31, -46, 38, -21, 16, 3, -43, -50, 4, 7]
  --                       [18, 16, -22, 4, -5, -46, -43, 28, 50, -47, 31, -41, 35,
  --                        -6, -20, -33, 10, 34, -7, -46, 0, 35, 29, 22, 19, -48,
  --                        -4, 10, -41, 26, -33, 45, -2, 24, 4, 39, -2, -42, 41,
  --                        18, -28, 28, -44, 19, 34, 41, 33, -27, -26, 41])
    where
    checkEq a b = putStrLn $ check a b
    check a b | a == b = "Correct."
    check e f = "Wrong. Expected " ++ (show e) ++ " but found " ++ (show f)
