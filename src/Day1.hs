module Day1
    ( readInts
    ) where


import System.IO ()
import Lib ( )


f :: [String] -> [Int]
f = map read


-- given list of integers, find the c integers that sum to n.
-- sumToN :: [Int] -> Int -> Int -> [[Int]]
-- sumToN [] _ _ = []
-- sumToN [_] 0 _ = [] -- sum of 0 ints == 0
-- sumToN xs c n = undefined

ff :: [Int] -> Int -> Maybe Int
ff [] _ = Nothing
ff (x:xs) c
    | c - x `elem` xs = Just (x * (c - x))
    | otherwise = ff xs c

ff' :: [Int] -> Int -> Int
ff' (x:xs) c = case ff xs (c - x) of
                    Nothing -> ff' xs c
                    Just y  -> y * x

-- >>> readInts ".\\Data\\Week1\\input1.txt"
readInts :: String -> IO ()
readInts fileName = do
        content <- readFile fileName
        let content_lines = f $ lines content 
        let y = ff' content_lines 2020
        print y
