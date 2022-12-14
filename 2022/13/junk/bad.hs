import System.IO
import System.IO.Unsafe
import Data.Char
import Text.Read

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        pairs = readPairs fileLines
        l = zip [1..] $ map compLists pairs

    putStr $ result l                                                                                                            
    hClose handle

result :: Show a => [a] -> String
result xs = unlines $ map show xs



contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents
pairs = readPairs fileLines

type Pair = (String,String)

readPairs :: [String] -> [Pair]
readPairs (x:y:[]) = [(x,y)]
readPairs (x:y:"":xs) = (x,y) : readPairs xs

readFirstInt :: String -> Maybe Int
readFirstInt "" = Nothing
readFirstInt l@(x:xs) 
    | (not . isDigit) x = readFirstInt xs
    | otherwise = (readMaybe (takeWhile (/=',') l) :: Maybe Int)

isEmpty :: String -> Bool
isEmpty str = (length $ filter isDigit str) == 0

countDepth :: String -> Int
countDepth str = length $ takeWhile (=='[') str

compLists :: Pair -> Bool
compLists (left,"") = left == ""
{-
compLists ("",right) = right /= ""
compLists ("","") = True
-}
compLists (left,right)
--  | (left == "[]") && (right == "[]") = compLists (lrest,rrest)
    | ((take 1 left) == "[") && ((take 1 right) == "[") = compLists (l1,r1)
    | (take 1 left) == "[" = compLists (l1,right)
    | (take 1 right) == "[" = compLists (left,r1)
--  | (isEmpty l1) && (isEmpty r1) = case (countDepth l1, countDepth r1) of
--      (n1,n2) | n1 == n2 -> compLists (lrest,rrest)
--      (n1,n2) -> n2 >= n1
    | otherwise = case ((readFirstInt left),(readFirstInt right)) of
        ((Just x),Nothing) -> False
        (Nothing,(Just x)) -> True
--      (Nothing,Nothing) -> compLists (tail (dropWhile (/=',') lrest),tail (dropWhile (/=',') rrest))
        (Nothing,Nothing) -> compLists (lscr,rscr)
        (Just x,Just y) | x == y -> case (dropWhile (/=',') left, dropWhile (/=',') right) of
            (',':ls,',':rs) -> compLists (ls,rs)
            (',':ls,rs) -> compLists (ls,rrest)
            (ls,',':rs) -> compLists (lrest,ls)
            (ls,rs) -> compLists (lrest,rrest)
        (Just x,Just y) -> y > x
    where
        (l1,lrest) = oneDir left         
        (r1,rrest) = oneDir right
        lscr = case (dropWhile (/=',') lrest) of
            "" -> ""
            xs -> tail xs
        rscr = case (dropWhile (/=',') rrest) of
            "" -> ""
            xs -> tail xs

tillList :: String -> String
tillList = dropWhile (/='[')

oneDir :: String -> Pair
oneDir lines = case listInit of
    [] -> ("","")
    xs -> (firstList,stripDir outerList)
    where
        (beforeList,listInit) = span (/='[') lines
        firstList = stripDir $ getDir (tail listInit) 1
        rest = drop ((length beforeList) + 1 + (length firstList)) listInit
        outerList = case rest of 
            "" -> beforeList
            ys -> concat [beforeList,(init ys)]
{-
        rest
            | rest' == "" = ""
            | (last rest') /= "" = rest ++ "]"
            | otherwise = rest
-}

stripDir :: String -> String
stripDir = id
{-
stripDir str 
    | str == "" = ""
    | ((head str) == '[') && ((last str) == ']') = stripDir $ init $ (drop 1 str)
    | otherwise = str
-}

getDir :: String -> Int -> String
getDir [] n = []
getDir (x:xs) 0 = []
getDir (x:xs) n = case x of
    ']' | n == 1 -> getDir xs (n-1)
    ']' -> x : getDir xs (n-1)
    '[' -> x : getDir xs (n+1)
    y -> x : getDir xs n

sumTind :: [Bool] -> Int
sumTind l = sum $ map fst $ filter (\(x,y) -> y) $ zip [1..] l


