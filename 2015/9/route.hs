import Data.List
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        dict = getDict fileLines
        names = nub $ allNames dict
        possibilities = permutations names
        allRoutes = map (calcRoute dict) possibilities
        minRoute = minimum allRoutes
        maxRoute = maximum allRoutes
    putStr $ result [minRoute,maxRoute]
    hClose handle


result :: Show a => [a] -> String
result xs = unlines $ map show xs

type Database = [(String,String,Int)]

getDict :: [String] -> Database
getDict [] = []
getDict (x:xs) = case (words x) of
    (name1:"to":name2:"=":dist:[]) -> (name1,name2,(read dist :: Int)) : getDict xs

getDist :: Database -> String -> String -> Int
getDist [] _ _ = error $ "Not found"
getDist ((n1,n2,d):xs) name1 name2 
    | name1 == n1 && name2 == n2 = d
    | name1 == n2 && name2 == n1 = d
    | otherwise = getDist xs name1 name2
    
allNames :: Database -> [String]
allNames [] = []
allNames ((n1,n2,d):xs) = n1 : n2:  allNames xs

calcRoute :: Database -> [String] -> Int
calcRoute dict (x:y:[]) = getDist dict x y
calcRoute dict (x:y:xs) = getDist dict x y + calcRoute dict (y:xs)
