module Day3(
        day3
        ) where

import System.IO ()
import Lib ( )
import Data.List

-- slope = 3 right 1 down

-- goal: find number of slope steps to take before out-of-bounds and how many of
    -- steps landed on a tree #
-- ! Note: all lines are cyclic to infinitey 

-- counts the number trees in a list of strings
checkTree :: Char -> Int
checkTree '#' = 1
checkTree _ = 0

part1 :: [String] -> Int -> Int -> Int -> Int -> Int -> Int
part1 [] _ _ _ _ acc = acc
part1 xs x0 y0 x y acc
    | y >= length xs = acc
    | otherwise =
        let
            a = xs !! y
            t = cycle a !! x
            tree = checkTree t
        in
            part1 xs x0 y0 (x+x0) (y+y0) (acc+tree)


day3 :: String -> IO ()
day3 fileName = do
        content <- readFile fileName
        let content_lines = lines content
        let out = part1 content_lines 3 1 3 1 0
        putStrLn $ "part 1 ans: " ++ show out
        let in2 = [(1,1),(3,1),(5,1),(7,1),(1,2)]
        let out2 = foldr ((*) . (\ (x, y) -> part1 content_lines x y x y 0)) 1 in2
        putStrLn $ "part 2 ans: " ++ show out2

        putStrLn "Done."