module Day14(
        day14
        ) where

import System.IO ()
import Data.List.Split ( split, startsWith )
import Data.Vector ()
import Data.List ( foldl', intercalate )
-- import Data.Array.IO ()
import Data.Char (digitToInt)
-- import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe ( isJust, fromJust )
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
-- 1. Convet int to 36 digit binary string
-- 2. Apply mask to binary string
-- 3. Convert new binary string to Int
-- 4. Create IntMap (dictionary) where the memory loc is key


-- newtype Memory = Memory (IntMap Int)

toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]

appendZeros :: String -> String
appendZeros xs
    | length xs < 36 = appendZeros ("0" ++ xs)
    | otherwise = xs

intToMask :: String -> String
intToMask xs = appendZeros $ intercalate "" $ map show $ toBin (read xs :: Int)

maskToInt :: String -> String
maskToInt x = show $ foldl' (\ acc x -> acc * 2 + digitToInt x) 0 x

applyMask :: String -> String -> String -> String
applyMask _ [] acc = acc
applyMask [] _ acc = acc
applyMask (x:xs) (y:ys) acc
    | x == '0' = applyMask xs ys (acc ++ "0")
    | x == '1' = applyMask xs ys (acc ++ "1")
    | otherwise = applyMask xs ys (acc ++ [y])


loop :: [String] -> Int
loop = undefined

main :: [[String]] -> IM.IntMap Int -> IM.IntMap Int
main (x:xs) = undefined

------------
-- Parser --
------------

number :: Parser Integer
number = do{ ds <- many1 digit
            ; return (read ds)
            }
            <?> "number"

mem :: Parser Integer
mem = do{ letter
        ; do{ 
            char '['
            ; number
            }
        <|> mem
        }  

-- >>>  parseTest mem "mem[17610] = 1035852"
-- *** Exception: C:\Users\giess\OneDrive\Documents\MyProjects\AdventOfCode2020\aoc\src\Day14.hs:72:15-23: error:
--     * Couldn't match expected type `Text.Parsec.Prim.ParsecT
--                                       String () Data.Functor.Identity.Identity Integer'
--                   with actual type `P.GenTokenParser s0 u0 m0
--                                     -> Text.Parsec.Prim.ParsecT s0 u0 m0 Integer'
--     * Probable cause: `P.integer' is applied to too few arguments
--       In a stmt of a 'do' block: P.integer
--       In the first argument of `(<|>)', namely
--         `do char '['
--             P.integer'
--       In a stmt of a 'do' block:
--         do char '['
--            P.integer
--           <|> mem
-- (deferred type error)
--



-- >>> day14 ".\\Data\\Day14\\day14.txt"
-- [["mask = 11110100010101111011001X0100XX00100X","mem[17610] = 1035852","mem[55284] = 229776690","mem[16166] = 12685380","mem[8340] = 16011"],["mask = 0X1X0X010101011X10X101000X0001110100","mem[968] = 15992","mem[32758] = 7076","mem[30704] = 1701","mem[33719] = 58012","mem[20818] = 25927237","mem[16718] = 46485"]]
-- Done.
--
day14 :: String -> IO ()
day14 fileName = do
        content <- readFile fileName

        -- Split data into chunks of 1 mask and n memory assignments
        let chunks = map lines $ split (startsWith "mask") content
        print $ take 2 chunks

        -- Part 1
        let out = main chunks IM.empty


        -- Test
        -- let memory = IM.insert 5 1 IM.empty
        -- let x = memory IM.!? 5
        -- print $ fromJust x

        putStrLn "Done."