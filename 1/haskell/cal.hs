import Data.List
import System.IO

main = do  
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        sumCalList = sumCalAll fileLines
        maxCal = maximum sumCalList
        sumMax3 = sumLast3 $ sort sumCalList
        
    putStr $ show (maximum sumCalList) ++ "\n" ++ show sumMax3 ++ "\n"
    hClose handle

sumCal :: [String] -> Int
sumCal = sum . map (\x -> read x :: Int) . takeWhile (/="")

sumCalAll :: [String] -> [Int]
sumCalAll [] = []
sumCalAll xs = sumCal xs : ( sumCalAll $ dropWhile (=="") $ dropWhile (/="") xs )

{-
sort' :: Ord a => [a] -> [a]
sort' (x:[]) = [x]
sort' (x:xs) = sort [ y | y <- xs, y < x ] ++ [x] ++ ( sort [ y | y <- xs, y >= x ] )
-}

sumLast3 (x:y:z:[]) = x + y + z
sumLast3 (x:y:z:w:xs) = sumLast3 $ y:z:w:xs
