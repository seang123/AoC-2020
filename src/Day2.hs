module Day2
    ( day2
    ) where


import System.IO ()
import Lib ( )
import qualified Data.Text as T

-- function to check a single password :: return True(pass) or Fales(fail)
-- function to loop through all passwords, inc counter for true passwords


repl :: Char -> Char
repl ':' = ' '
repl '-' = ' '
repl  c  = c

-- >>> cleanString "1-2 a: aabccc"
-- ["1","2","a","aabccc"]
--
cleanString :: [Char] -> [String]
cleanString xs = words $ map repl xs


f :: [String] -> [[String]]
f xs = [[x] | x <- xs]

-- count char occurance in string
chars :: String -> Char -> Int -> Int
chars [] y acc = acc
chars (x:xs) y acc
    | x == y = chars xs y (acc+1)
    | otherwise = chars xs y acc

-- check password validity (1 legal | 0 illigal)
-- >>> validPassword ["13","17","s","ssssssssssssgsssj"]
-- 1
--
validPassword :: [String] -> Int
validPassword [] = 0
validPassword (x:y:z:str) =
    let zz = head z :: Char 
        s = head str :: String
        c = chars s zz 0
        xx = read x :: Int
        yy = read y :: Int
    in 
        (if (c < xx) || (c > yy) then 0 else 1)

-- check passwords using new rule (Part 2)
validPassword2 :: [String] -> Int
validPassword2 [] = 0
validPassword2 (x:y:z:str) 
    | (s1 == zz) /= (s2 == zz) = 1
    | otherwise = 0
    where
        zz = head z :: Char    -- the char
        s = head str :: String -- the password
        xx = read x :: Int
        yy = read y :: Int
        s1 = s !! (xx-1)
        s2 = s !! (yy-1)


-- >>> day2 ".\\Data\\Day2\\day2.txt"
-- ["13","17","s","ssssssssssssgsssj"]
-- 275
--
day2 :: String -> IO ()
day2 fileName = do
        content <- readFile fileName
        let content_lines = map (concatMap cleanString) (f $ lines content)
        print $ head content_lines
        let passwords = map validPassword2 content_lines
        let count = sum passwords
        print count

