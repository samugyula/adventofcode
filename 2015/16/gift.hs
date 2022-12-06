{-# LANGUAGE InstanceSigs #-}                                                                                                    
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    handle2 <- openFile "data2.txt" ReadMode
    contents <- hGetContents handle
    contents2 <- hGetContents handle2
    let
        fileLines = lines contents
        fileLines2 = lines contents2
        gift = readItem fileLines2
        aunts = readAunts fileLines
        res1 = res gift aunts (==)
        res2 = res gift aunts (check)

    putStr $ result [res1,res2]
    hClose handle
    hClose handle2

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


res :: Database -> [Database] -> (Item -> Item -> Bool) -> Int
res gift aunts f = nAunt
    where
        counts = map (count f gift) aunts
        maxMatch = maximum counts
        nAunt = 1 + (length $ takeWhile (/=maxMatch) counts)

type Item = (String,Int)
type Database = [Item]

readInt :: String -> Int
readInt xs = case (last xs) of
    ',' -> read (init xs) :: Int
    _   -> read xs :: Int

readItem :: [String] -> Database
readItem [] = []
readItem (line:xs) = case (words line) of   
    (name:val:[]) -> (init name, readInt val) : readItem xs

convertLine :: [String] -> [String]
convertLine (x:y:[]) = (x ++ " " ++ y) : []
convertLine (x:y:xs) = (x ++ " " ++ y) : convertLine xs

readAunts :: [String] -> [Database]
readAunts [] = []
readAunts (line:xs) = case (words line) of   
    (_:_:ys) -> (readItem . convertLine) ys : readAunts xs

check :: Item -> Item -> Bool
check (n1,v1) (n2,v2)
    | n1 == n2 = case n1 of
        x | x `elem` ["cats","trees"] -> v2 > v1
        x | x `elem` ["pomeranians","goldfish"] -> v2 < v1
        x -> v1 == v2
    | otherwise = False

count :: (Item -> Item -> Bool) -> Database -> Database -> Int
count _ [] _ = 0
count f (x:xs) db 
    | any (f x) db = 1 + count f xs db    
    | otherwise    = count f xs db
