import Data.List
import System.IO

main = do  
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        containList = map contained fileLines
        nContained = length $ filter id containList
        overlapList = map overlap fileLines
        nOverlap = length $ filter id overlapList
    putStr $ show nContained ++ "\n" ++ show nOverlap ++ "\n"
    hClose handle

breakOn :: Char -> String -> (String,String)
breakOn d str = (takeWhile (/= d) str, dropWhile (== d) (dropWhile (/= d) str))

rel :: (Int -> Int -> Bool) -> String -> String -> Bool
rel f x1 x2 = f (read x1 :: Int) (read x2 :: Int)

ge :: String -> String -> Bool
ge = rel (>=)

le :: String -> String -> Bool
le = rel (<=)

contained :: String -> Bool
contained str = ((ge min1 min2) && (le max1 max2)) || ((ge min2 min1) && (le max2 max1)) 
    where
        (first, second) = breakOn ',' str
        (min1,max1) = breakOn '-' first
        (min2,max2) = breakOn '-' second
        
overlap :: String -> Bool
overlap str = ((ge min1 min2) && (le min1 max2)) || ((ge min2 min1) && (le min2 max1)) 
    where
        (first, second) = breakOn ',' str
        (min1,max1) = breakOn '-' first
        (min2,max2) = breakOn '-' second
