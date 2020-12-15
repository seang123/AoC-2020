module Day5(
        day5
        ) where

import System.IO ()
import Data.List.Split
import Data.Sort ( sort )
import Data.List

-- find the row, given the first 7 characters of the boarding pass
-- BFFFBBF -> [0..127] => 70
findRow :: String -> [Int] -> Int
findRow [] ys = head ys
findRow (x:xs) ys = findRow xs $ splitRow x ys


splitRow :: Char -> [Int] -> [Int]
splitRow _ [] = []
splitRow c xs
    | c == 'F' || c == 'L' = head $ chunksOf (( length xs ) `div` 2) xs
    | c == 'B' || c == 'R' = chunksOf (( length xs ) `div` 2) xs !! 1
    | otherwise = []

-- RRR -> [0..7] => 7
findCol :: String -> [Int] -> Int
findCol [] ys = head ys
findCol (x:xs) ys = findCol xs $ splitRow x ys

findID :: [String]-> Int
findID [] = 0
findID xs = findRow (xs !! 0) [0..127] * 8 + findCol (xs !! 1) [0..7]

-- get elements in xs not in ys
findMissing :: [Int] -> [Int] -> [Int]
findMissing xs ys = xs \\ ys

-- >>> day5 ".\\Data\\Day5\\day5.txt"
day5 :: String -> IO ()
day5 fileName = do
        content <- readFile fileName -- each line is a boarding pass

        let content_lines = map (chunksOf 7) $ lines content -- split into row/col

        let ids = map findID content_lines

        print $ sort ids

        putStrLn $ "Part 1: " ++ show (maximum ids)

        let out = findMissing [0..maximum ids] ids

        putStrLn $ "Part 2: " ++ show out -- 682

        putStrLn "Done."