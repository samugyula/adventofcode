import Data.Char
import Data.List
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents

        -- get pairs
        pairs = readPairs fileLines

        -- sum indices of correct pairs
        l =  map cmp pairs
        res1 = sumTind l

        -- multiply indices of dividers
        packets = "[[2]]" : "[[6]]" : (filter (/="") fileLines)
        s = srt packets
        i1 = case (elemIndex (packets !! 0) s) of
            Just x -> x + 1
        i2 = case (elemIndex (packets !! 1) s) of
            Just x -> x + 1
        res2 = i1*i2

    putStr $ result [res1,res2]
    hClose handle

                                                                                                                                 
-- turn results into string
result :: Show a => [a] -> String
result xs = unlines $ map show xs

type Pair = (String,String)

-- read pairs
readPairs :: [String] -> [Pair]
readPairs (x:y:[]) = [(x,y)]
readPairs (x:y:"":xs) = (x,y) : readPairs xs

-- compare packets
-- mostly character-by-character
-- when number encountered, use the whole number
cmp :: Pair -> Bool
cmp ("","") = True
cmp ((x:xs),"") = False
cmp ("",(x:xs)) = True
cmp (l@(x:xs),r@(y:ys)) = case (x,y) of
    (',',',') -> cmp (xs,ys)
    (',',']') -> False
    (']',',') -> True
    ('[','[') -> cmp (xs,ys)
    (']',']') -> cmp (xs,ys)
    ('[',w) | isDigit w -> cmp (l, getDigListPass r)
    (v,'[') | isDigit v -> cmp (getDigListPass l, r)
    (']',w) | isDigit w -> True
    (v,']') | isDigit v -> False
    (v, w ) | (isDigit v) && (not (isDigit w)) -> False
    (v, w ) | (not (isDigit v)) && (isDigit w) -> True
    ('[',']') -> False
    (']','[') -> True
    (v, w ) | (isDigit v) && (isDigit w) -> case (read (dig l) :: Int, read (dig r) :: Int) of
        (n1,n2) | n1 == n2 -> cmp (dropWhile (not . ((flip elem) ",[]")) l, dropWhile (not . ((flip elem) ",[]")) r)
        (n1,n2) -> n2 >= n1

    where

        -- extract first number
        dig :: String -> String
        dig xs = takeWhile isDigit xs

        -- make string with the first number as list
        getDigListPass :: String -> String
        getDigListPass xs = '[':(dig xs) ++ "]" ++ rest
            where
                rest = drop (length $ dig xs) xs

-- sum indices of correct pairs
sumTind :: [Bool] -> Int
sumTind l = sum $ map fst $ filter (\(x,y) -> y) $ zip [1..] l

-- sort packets (quicksort based on cmp)
srt :: [String] -> [String]
srt [] = []
srt [x] = [x]
srt (x:xs) = srt [p | p <- xs, cmp (p,x)] 
              ++ [x] 
              ++  (srt [p | p <- xs, not (cmp (p,x))])
