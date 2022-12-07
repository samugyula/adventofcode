import Data.Char
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents

        listOfDirs = getDirList fileLines
        sizesOfDirs = map sum $ map sizes listOfDirs

        sumLimitedDirs = sum $ filter (<=100000) sizesOfDirs

        total  = 70000000
        needed = 30000000
        used = head sizesOfDirs 
        free = total - used
        needToFree = needed - free

        largeEnoughDirs = filter (>= needToFree) sizesOfDirs
        minEnoughDir = minimum largeEnoughDirs

    putStr $ result [sumLimitedDirs, minEnoughDir]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

sizes :: [String] -> [Int]
sizes lines = [read (head (words x)) :: Int | x <- lines, isDigit (head x)]

oneDir :: [String] -> [String]
oneDir lines = case tillcd of
    [] -> []
    xs -> getDir (tail tillcd) 1
    where
        tillcd = dropWhile (\x -> (take 4 x) /= "$ cd") lines

getDir :: [String] -> Int -> [String]
getDir [] n = []
getDir (x:xs) 0 = []
getDir (x:xs) n = case x of
    "$ cd .." | n == 1 -> getDir xs (n-1)
    "$ cd .." -> x : getDir xs (n-1)
    y | (take 4 y) == "$ cd" -> x : getDir xs (n+1)
    y -> x : getDir xs n

getAllDirs :: [String] -> [[String]]
getAllDirs [] = []
getAllDirs str = case one of
    [] -> getAllDirs rest
    ys -> ys : getAllDirs rest
    where
        one = oneDir str
        tillcd = takeWhile (\x -> (take 4 x) /= "$ cd") str
        rest = drop (1 + (length tillcd) + (length one)) $ tail str

getDirList :: [String] -> [[String]]
getDirList lines = dirs ++ (restdirs dirs)
    where
        dirs = getAllDirs lines
        restdirs [] = []
        restdirs (x:xs) = getDirList x ++ restdirs xs
        

