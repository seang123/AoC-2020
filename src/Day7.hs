module Day7(
        day7
        ) where

import System.IO ()


day7 :: String -> IO ()
day7 fileName = do
        content <- readFile fileName


        putStrLn "Done."