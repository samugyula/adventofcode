import System.IO.Unsafe
import Data.Char
import Text.Read

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
{-
compLists (left,"") = left == ""
compLists ("",right) = right /= ""
compLists ("","") = True
-}
compLists (left,right)
--  | (left == "[]") && (right == "[]") = compLists (lrest,rrest)
    | ((not . isEmpty) l1) && l1 == r1 = compLists (lrest,rrest)
    | (isEmpty l1) && (isEmpty r1) = case (countDepth l1, countDepth r1) of
        (n1,n2) | n1 == n2 -> compLists (lrest,rrest)
        (n1,n2) -> n2 >= n1
    | ((take 1 left) == "[") && ((take 1 right) == "[") = compLists (l1,r1)
    | (take 1 left) == "[" = compLists (l1,right)
    | (take 1 right) == "[" = compLists (left,r1)
    | otherwise = case ((readFirstInt left),(readFirstInt right)) of
        ((Just x),Nothing) -> False
        (Nothing,(Just x)) -> True
--      (Nothing,Nothing) -> compLists (tail (dropWhile (/=',') lrest),tail (dropWhile (/=',') rrest))
        (Nothing,Nothing) -> compLists (lscr,rscr)
        (Just x,Just y) | x == y -> case (dropWhile (/=',') left, dropWhile (/=',') right) of
            (',':ls,',':rs) -> compLists (ls,rs)
            (',':ls,rs) -> compLists (ls,rrest)
            (ls,',':rs) -> compLists (lrest,rs)
            (ls,rs) -> compLists (lrest,rrest)
        (Just x,Just y) -> y > x
    where
        (l1',lrest) = oneDir left         
        (r1',rrest) = oneDir right
        (l1,r1) = if ((l1' == "") && ((r1' == "")))
            then (lrest,rrest)
            else (l1',r1')
{-
        l1
            | l1' == "" = lrest
            | otherwise = l1'
        r1
            | r1' == "" = rrest
            | otherwise = r1'
-}
        lscr = case (dropWhile (/=',') lrest) of
            "" -> ""
            xs -> tail xs
        rscr = case (dropWhile (/=',') rrest) of
            "" -> ""
            xs -> tail xs

compLists' :: Pair -> [Pair]
{-
compLists' (left,"") = left == ""
compLists' ("",right) = right /= ""
compLists' ("","") = True
-}
compLists' (left,right)
--  | (left == "[]") && (right == "[]") = compLists' (lrest,rrest)
    | ((not . isEmpty) l1) && l1 == r1 = lr : (compLists' (lrest,rrest))
    | (isEmpty l1) && (isEmpty r1) = case (countDepth l1, countDepth r1) of
        (n1,n2) | n1 == n2 -> [(lscr,rscr)] -- : (compLists' (lrest,rrest))
        (n1,n2) -> [lr] --n2 >= n1
    | ((take 1 left) == "[") && ((take 1 right) == "[") = lr : (compLists' (l1,r1))
    | (take 1 left) == "[" = lr : (compLists' (l1,right))
    | (take 1 right) == "[" = lr : (compLists' (left,r1))
    | otherwise = case ((readFirstInt left),(readFirstInt right)) of
        ((Just x),Nothing) -> [lr] --False
        (Nothing,(Just x)) -> [lr] --True
--      (Nothing,Nothing) -> compLists' (tail (dropWhile (/=',') lrest),tail (dropWhile (/=',') rrest))
        (Nothing,Nothing) -> lr : (compLists' (lscr,rscr))
        (Just x,Just y) | x == y -> case (dropWhile (/=',') left, dropWhile (/=',') right) of
            (',':ls,',':rs) -> lr : (compLists' (ls,rs))
            (',':ls,rs) -> lr : (compLists' (ls,rrest))
            (ls,',':rs) -> lr : (compLists' (lrest,rs))
            (ls,rs) -> lr : (compLists' (lrest,rrest))
        (Just x,Just y) -> [lr] --y > x
    where
        lr = (left,right)
        (l1',lrest) = oneDir left         
        (r1',rrest) = oneDir right
        (l1,r1) = if ((l1' == "") && ((r1' == "")))
            then (lrest,rrest)
            else (l1',r1')

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
    [] -> (lines,"")
    xs -> res
    where
        (beforeList,listInit) = span (/='[') lines
        firstList = getDir (tail listInit) 1
        rest = drop ((length beforeList) + 1 + (length firstList)) listInit
        outerList = case rest of 
            "" -> beforeList
            ys -> case ys of
                (']':',':zs) | (last zs) == ']' -> concat [beforeList,(init zs)]
                (']':',':zs) -> concat [beforeList,zs]
                zs -> concat [beforeList,(init zs)]
{-
        res
            | (outerList == "") && ((take 1 firstList) == "[") = oneDir firstList
            | otherwise = (firstList, outerList)
-}
        res = (firstList, outerList)
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


