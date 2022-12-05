import Data.List
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        db = makeDatabase fileLines
        newDb = extendDatabase db $ getNames db
        p1 = getRes db
        p2 = getRes newDb
    putStr $ result [p1,p2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

getRes :: Database -> Int
getRes db = maxPoint
    where
        names = getNames db
        seatings = permutations names
        seatings1 = map (\x -> x ++ [head x]) seatings
        points = map (countPoints db) seatings1
        maxPoint = maximum points

type Database = [(String,String,Int)]

makeDatabase :: [String] -> Database
makeDatabase [] = []
makeDatabase (line:xs) = case (words line) of
    (name1:_:"gain":num:_:_:_:_:_:_:name2:[]) -> (name1,init name2,  read num :: Int)  : makeDatabase xs
    (name1:_:"lose":num:_:_:_:_:_:_:name2:[]) -> (name1,init name2,-(read num :: Int)) : makeDatabase xs

getVal :: Database -> String -> String -> Int
getVal ((n1,n2,p):xs) name1 name2
    | n1 == name1 && n2 == name2 = p
    | otherwise = getVal xs name1 name2

getNames :: Database -> [String]
getNames db = nub [ n1 | (n1,_,_) <- db ]

countPoints :: Database -> [String] -> Int
countPoints _  (x:[]) = 0
countPoints db (x:y:xs) = getVal db x y + ( getVal db y x) + ( countPoints db (y:xs))

extendDatabase :: Database -> [String] -> Database
extendDatabase db [] = db
extendDatabase db (name:xs) = ("Self",name,0) : (name,"Self",0) : extendDatabase db xs
