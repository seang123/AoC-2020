module Day10(
        day10
        ) where

import System.IO ()
import Data.Sort ( sort )

-- count number of differences ==1 or ==3
count :: [Int] -> Int -> Int -> (Int, Int)
count [] x y = (x, y)
count [_] x y = (x, y)
count (x:y:xs) a b 
    | y - x == 1 = count (y:xs) (a+1) b
    | y - x == 3 = count (y:xs) a (b+1)
    | otherwise = count (y:xs) a b


prod :: (Int, Int) -> Int
prod (x,y) = x * y

tribonacci :: [Int] -> [Int] -> [Int]
tribonacci [] ys = ys
tribonacci xs ys = 
        let 
                three = take 3 xs
                new = sum three
        in
                tribonacci (drop 1 xs) (ys ++ three ++ [new])

-- tribonacci
-- [0, 1, 1, 2, 4, 7, 13, 24, ...]

-- >>> day10 ".\\Data\\Day10\\day10.txt"
-- 2346
-- [1,2,3,4,7,10,11,12,15,16,17,18,19,22,25,26,27,28,31,32,35,38,39,40,41,44,45,46,47,48,51,54,55,56,57,60,63,64,65,66,69,70,71,72,75,78,79,82,83,84,85,86,89,90,91,92,95,98,99,100,103,104,105,108,109,110,113,114,115,118,119,120,121,122,125,126,127,128,129,132,133,134,135,138,139,142,143,144,147,150,151,152,153,154,157,158,159,160,161,164,167,168]
-- ""
-- [0,1,1,1,1,3,3,1,1,3,1,1,1,1,3,3,1,1,1,3,1,3,3,1,1,1,3,1,1,1,1,3,3,1,1,1,3,3,1,1,1,3,1,1,1,3,3,1,3,1,1,1,1,3,1,1,1,3,3,1,1,3,1,1,3,1,1,3,1,1,3,1,1,1,1,3,1,1,1,1,3,1,1,1,3,1,3,1,1,3,3,1,1,1,1,3,1,1,1,1,3,3,1,3]
-- [0,1,1,2,1,1,1,3,1,1,1,3,1,1,3,5,1,3,3,7,3,3,1,7,3,1,1,5,1,1,3,5,1,3,1,5,3,1,1,5,1,1,1,3,1,1,1,3,1,1,3,5,1,3,3,7,3,3,1,7,3,1,1,5,1,1,1,3,1,1,3,5,1,3,1,5,3,1,3,7,1,3,3,7,3,3,1,7,3,1,1,5,1,1,1,3,1,1,3,5,1,3,1,5,3,1,1,5,1,1,1,3,1,1,1,3,1,1,3,5,1,3,3,7,3,3,1,7,3,1,1,5,1,1,1,3,1,1,3,5,1,3,3,7,3,3,1,7,3,1,1,5,1,1,1,3,1,1,3,5,1,3,1,5,3,1,1,5,1,1,1,3,1,1,3,5,1,3,3,7,3,3,1,7,3,1,3,7,1,3,1,5,3,1,1,5,1,1,1,3,1,1,1,3,1,1,3,5,1,3,1,5,3,1,1,5,1,1,1,3,1,1,3,5,1,3,3,7,3,3,1,7,3,1,1,5,1,1,3,5,1,3,1,5,3,1,1,5,1,1,3,5,1,3,1,5,3,1,1,5,1,1,3,5,1,3,1,5,3,1,1,5,1,1,3,5,1,3,1,5,3,1,1,5,1,1,1,3,1,1,1,3,1,1,3,5,1,3,1,5,3,1,1,5,1,1,1,3,1,1,1,3,1,1,3,5,1,3,1,5,3,1,1,5,1,1,1,3,1,1,3,5,1,3,1,5,3,1,3,7,1,3,1,5,3,1,1,5,1,1,3,5,1,3,3,7,3,3,1,7,3,1,1,5,1,1,1,3,1,1,1,3,1,1,3,5,1,3,1,5,3,1,1,5,1,1,1,3,1,1,1,3,1,1,3,5,1,3,3,7,3,3,1,7,3,1,3,7,1,3,4,3,3]
-- Done.
--
day10 :: String -> IO ()
day10 fileName = do
        content <- readFile fileName

        let content_lines = lines content

        let numbers = sort $ map (read :: String -> Int) content_lines
        print $ prod $ count (sort numbers) ( head numbers ) 1

        print numbers

        -- Part 2

        let n = [0] ++ numbers ++ [last numbers + 3]

        let n2 = 0 : map (\ x -> x * (- 1)) (zipWith (-) n (tail n))

        print ""
        print n2

        print $ tribonacci n2 []



        putStrLn "Done."