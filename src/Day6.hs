module Day6(
        day6
        ) where

import System.IO ()
import Data.List.Split
import Data.Sort ( sort )
import Data.List

-- intersection between a list of elments
common :: Eq a => [[a]] -> [a]
common (x:xs) = foldr intersect x xs

-- >>> day6 ".\\Data\\Day6\\day6.txt"
-- 6430
-- 3125
-- Done.
--
day6 :: String -> IO ()
day6 fileName = do
        content <- readFile fileName
        let forms = map (splitOneOf "\n ") $ splitOn "\n\n" content

        -- Part 1
        print $ sum $ map (length . nub . concatMap nub) forms

        -- Part 2
        print $ foldr ((+) . length . common . map nub) 0 forms

        putStrLn "Done."