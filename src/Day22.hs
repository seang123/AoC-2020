module Day22(
        day22
        ) where

import System.IO ()
import Data.List (nub)
import qualified Data.IntMap as IM
import Data.Maybe ( isJust, fromJust )
import qualified Data.Set as S

-- calculate the output value
sum_ :: (Num a, Enum a) => [a] -> a
sum_ xs = sum $ zipWith (*) [1..] $ reverse xs

-- Part 1
play :: [Int] -> [Int] -> Int
play dA dB = 
    let 
        dC = fromJust $ play' dA dB
    in 
        sum_ dC
    where
        play' :: [Int] -> [Int] -> Maybe [Int]
        play' [] y = Just y
        play' x [] = Just x
        play' (x:xs) (y:ys)
            | x > y = play' (xs ++ [x, y]) ys
            | x < y = play' xs (ys ++ [y, x]) 
            | otherwise = Nothing -- shouldn't trigger according to rule 

-- Part 2
playRec :: [Int] -> [Int] -> [([Int], [Int])] -> (Int,Int)
playRec xs [] _ = (sum_ xs, 1)
playRec [] ys _ = (sum_ ys, 2)
playRec (x:xs) (y:ys) zs 
    | (x:xs,y:ys) `elem` zs = (sum_ xs, 1)
    | x > length xs || y > length ys = if x > y then playRec (xs ++ [x,y]) ys (zs ++ [(x:xs, y:ys)])
                                        else playRec xs (ys++[y,x]) (zs ++ [(x:xs, y:ys)])
    | otherwise = case playRec (take x xs) (take y ys) [] of
                    (_, 1) -> playRec (xs ++ [x,y]) ys (zs ++ [(x:xs, y:ys)])
                    (_, 2) -> playRec xs (ys++[y,x]) (zs ++ [(x:xs, y:ys)])

-- >>> day22 ".\\Data\\Day14\\day14.txt"
-- ProgressCancelledException
day22 :: String -> IO ()
day22 fileName = do
        content <- readFile fileName

        let deckA = [43,36,13,11,20,25,37,38,4,18,1,8,27,23,7,22,10,5,50,40,45,26,15,32,33]
        let deckB = [21,29,12,28,46,9,44,6,16,39,19,24,17,14,47,48,42,34,31,3,41,35,2,30,49]
        putStrLn $ "1 -> " ++ show (  play deckA deckB )


        let testA = [9, 2, 6, 3, 1]
        let testB = [5, 8, 4, 7, 10] -- expect output == 291
        -- Part 2
        putStrLn $ "Test (should = 291) " ++ show ( playRec testA testB [] )
        putStrLn $ "2 -> " ++ show ( playRec deckA deckB [] )


        putStrLn "Done."
