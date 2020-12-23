module Day22(
        day22
        ) where

import System.IO ()
import Data.List ()
import qualified Data.IntMap as IM
import Data.Maybe ( isJust, fromJust )

sum_ :: (Num a, Enum a) => [a] -> a
sum_ xs = sum $ zipWith (*) xs (reverse $ take (length xs) [1..])

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
            | x > y = play' (xs ++ [x] ++ [y]) ys
            | x < y = play' xs (ys ++ [y] ++ [x]) 
            | otherwise = Nothing -- shouldn't trigger according to rule 


-- playRec :: [Int] -> [Int] -> [(Int, Int)] -> [Int]
-- playRec xs ys zs = playRec' xs ys zs
--     where
--         playRec' :: [Int] -> [Int] -> [(Int, Int)] -> [Int]
--         playRec' [] ys _ = ys
--         playRec' xs [] _ = xs
--         playRec' (x:xs) (y:ys) zs 
--             | (x,y) `elem` zs = playRec (xs ++ [x] ++ [x]) ys (zs ++ [(x,y)])
--             | otherwise = helper (x:xs) (y:ys) (zs ++ [(x,y)])
--                 where
--                     helper :: [Int] -> [Int] -> [(Int, Int)] -> [Int]
--                     helper (x:xs) (y:ys) zs
--                         | (x >= length xs) && (y >= length ys) = playRec' (take x xs) (take y ys) zs -- recursive entry here (currently its throwing away x and y)
--                         | x < length xs = if x > y then playRec xs (ys ++ [x] ++ [y]) zs else playRec xs (ys ++ [y] ++ [x]) zs
--                         | y < length ys = if x > y then playRec (xs ++ [x] ++ [y]) ys zs else playRec (xs ++ [y] ++ [x]) ys zs
--                         | otherwise = playRec xs ys zs

-- >>> [(1,0): [(2,1),(3,7),(4,5)]]
-- [[(1,0),(2,1),(3,7),(4,5)]]
--

data Output = Output Int [(Int, Int)]

playRec :: [Int] -> [Int] -> [(Int, Int)] -> Int
playRec [] ys _ = sum_ ys
playRec xs [] _ = sum_ xs
playRec (x:xs) (y:ys) zs 
    | winner == 1 && x > y = playRec (xs ++ [x] ++ [y]) ys (nzs ++ [(x,y)])
    | winner == 1 && x < y = playRec (xs ++ [y] ++ [x]) ys (nzs ++ [(x,y)])
    | winner == 2 && x > y = playRec xs (ys ++ [x] ++ [y]) (nzs ++ [(x,y)])
    | winner == 2 && x < y = playRec xs (ys ++ [y] ++ [x]) (nzs ++ [(x,y)])
    | otherwise = 0
    where 
        Output winner nzs = playRec' (x:xs) (y:ys) zs


-- ret: 1 if p1 wins or 2 if p2 wins
playRec' :: [Int] -> [Int] -> [(Int, Int)] -> Output
playRec' xs [] zs = Output 1 zs
playRec' [] ys zs = Output 2 zs
playRec' (x:xs) (y:ys) zs 
    | (x,y) `elem` zs = Output 1 zs -- rule 1
    | x < length xs && y < length ys = if x > y then Output 1 zs else Output 2 zs
    | x < length xs && y >= length ys = if x > y then Output 1 zs else Output 2 zs
    | x >= length xs && y < length ys = if x > y then Output 1 zs else Output 2 zs
    | otherwise = playRec' (take x xs) (take y ys) (zs ++ [(x,y)]) -- rec case

-- 32096 old value p2

-- >>> day22 ".\\Data\\Day14\\day14.txt"
-- 1 -> 30780
-- 2 -> 34340
-- Done.
--
day22 :: String -> IO ()
day22 fileName = do
        content <- readFile fileName

        let deckA = [43,36,13,11,20,25,37,38,4,18,1,8,27,23,7,22,10,5,50,40,45,26,15,32,33]
        let deckB = [21,29,12,28,46,9,44,6,16,39,19,24,17,14,47,48,42,34,31,3,41,35,2,30,49]
        putStrLn $ "1 -> " ++ show (  play deckA deckB )

        -- Part 2
        putStrLn $ "2 -> " ++ show ( playRec deckA deckB [] )


        putStrLn "Done."
