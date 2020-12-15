module Day4(
        day4
        ) where

import System.IO ()
import Data.List.Split

data Passport = Passport{
          byr :: Maybe Int
        , iyr :: Maybe Int
        , eyr :: Maybe Int
        , hgt :: Maybe Int
        , hcl :: Maybe Int
        , ecl :: Maybe Int
        , pid :: Maybe Int
        , cid :: Maybe Int
} deriving(Show, Eq)

-- reading data with lines returns a list with "" seperating the different passports

-- 1 if cid, 0 otherwise
checkForCID :: [String] -> Int
checkForCID [] = 0
checkForCID (x:xs)
        | length (x:xs) == 8 = 1
        | field == "cid" = 1
        | otherwise = checkForCID xs
        where
                field = head $ splitOn ":" x
                   

validatePassport :: [String] -> Int
validatePassport [] = 0
validatePassport xs 
        | length xs == 8 = 1
        | length xs == 7 = 1 - checkForCID xs
        | otherwise = 0


day4 :: String -> IO ()
day4 fileName = do
        content <- readFile fileName
        let passports = map (splitOneOf "\n ") $ splitOn "\n\n" content

        print $ head passports

        let valid_passports = map validatePassport passports
        print $ sum valid_passports

        putStrLn "Done."

-- "byr:2001 iyr:2011\necl:brn\npid:487702556 hcl:#602927\nhgt:167cm eyr:2026"


-- >>> splitOn "\n" ["eyr:2029 pid:157374862","byr:1991 ecl:amb hcl:#a97842 hgt:178cm","","byr:1962 pid:547578491 eyr:2028 ecl:hzl hgt:65in iyr:2013 hcl:#623a2f"]-- Variable not in scope: splitOn :: [Char] -> [[Char]] -> t
-- Variable not in scope: splitOn :: [Char] -> [[Char]] -> t
