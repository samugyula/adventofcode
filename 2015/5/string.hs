import System.IO
import Data.List

main = do
    handler <- openFile "data.txt" ReadMode
    contents <- hGetContents handler
    let
        fileLines = lines contents
        nNice = length $ filter id $ map nice fileLines
        nNice2 = length $ filter id $ map nice2 fileLines
    putStr $ show nNice ++ "\n" ++ show nNice2 ++ "\n"
    hClose handler

countVowels :: String -> Int
countVowels str = length [x | x <- str, x `elem` "aeiou"]

lookDouble :: String -> Bool
lookDouble (x:[])   = False
lookDouble (x:y:xs)
    | x == y    = True
    | otherwise = lookDouble $ y:xs

lookForbidden :: String -> Bool
lookForbidden (x:[])   = False
lookForbidden (x:y:xs) = case (x:y:[]) of 
    "ab" -> True
    "cd" -> True
    "pq" -> True
    "xy" -> True
    _    -> lookForbidden $ y:xs

nice :: String -> Bool
nice str = (countVowels str) >= 3 && lookDouble str && (not ( lookForbidden str))

lookPair :: String -> Bool
lookPair (x:[])   = False
lookPair (x:y:xs)
    | isInfixOf (x:y:[]) xs = True
    | otherwise             = lookPair (y:xs)

lookRepeatWithBreak :: String -> Bool
lookRepeatWithBreak (x:y:[])   = False
lookRepeatWithBreak (x:y:z:xs) 
    | x == z    = True
    | otherwise = lookRepeatWithBreak (y:z:xs)

nice2 :: String -> Bool
nice2 str = (lookPair str) && (lookRepeatWithBreak str)
