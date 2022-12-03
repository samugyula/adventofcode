import Data.List
import System.IO

main = do  
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let 
        fileLines = lines contents
        groupList = groups fileLines
        sumPrior = sum $ getPrior $ map sects groupList
    putStr $  show sumPrior ++ "\n"
    hClose handle

groups :: [String] -> [(String, String, String)]
groups (x:y:z:lines) = (x, y, z) : (groups lines)
groups _             = []

sects :: (String, String, String) -> Char
sects (x,y,z) = (nub $ intersect x $ intersect y z) !! 0

priorities :: [(Char,Int)]
priorities = zip (['a'..'z'] ++ ['A'..'Z']) [1..]

getVal :: [(Char,Int)] -> Char -> Int
getVal ((key,val):xs) x
    | x == key  = val
    | otherwise = getVal xs x

getPrior :: [Char] -> [Int]
getPrior = map (getVal priorities)
