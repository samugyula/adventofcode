import Data.List
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        res1 = length $ getTillUnique 4 contents
        res2 = length $ getTillUnique 14 contents

    putStr $ result [res1,res2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


getTillUnique :: Int -> String -> String
getTillUnique n l@(x:xs) 
    | length toCheck == length (nub toCheck) = toCheck
    | otherwise = x : getTillUnique n xs
    where
        toCheck = take n l
