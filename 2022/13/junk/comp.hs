import System.IO.Unsafe
import Data.Char
import Data.List
import Text.Read
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        pairs = readPairs fileLines
--      l = zip [1..] $ map compLists pairs
        l = zip [1..] $ map cmp pairs
        packets = "[[2]]" : "[[6]]" : (filter (/="") fileLines)
        s = sortPackets packets

    putStr $ result l
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents
pairs = readPairs fileLines

packets = "[[2]]" : "[[6]]" : (filter (/="") fileLines)
--packets =  (filter (/="") fileLines)

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

list :: String -> [String]
list [] = []
list xs = (takeWhile (/=',') xs) : (list (dropWhile (==',') (dropWhile (/=',') xs)))

toList :: String -> String
toList str = '[':(intercalate "],["  (list str)) ++ "]"

compLists :: Pair -> Bool
compLists (left,right)
--  | (l1 == r1) && (lrest == rrest) = True
    | ((getAllDirs left) /= []) && ((getAllDirs right) /= []) && ((getAllDirs left) /= (getAllDirs right)) && (isEmpty left) && (isEmpty right) 
            = length ((head (getAllDirs left))) <= (length (( head (getAllDirs right))))
    | (length (take 1 left) == 1) && ((take 1 left) /= "[") && ((take 2 right) == "[]") = False
    | (length (take 1 right) == 1) && ((take 1 right) /= "[") && ((take 2 left) == "[]") = True
    | ((not . isEmpty) l1) && l1 == r1 = compLists (lrest,rrest)
    | (isEmpty l1) && (isEmpty r1) = case (countDepth l1, countDepth r1) of
        (n1,n2) | n1 == n2 -> True --compLists (lrest,rrest)
        (n1,n2) -> n2 >= n1
    | ((take 1 left) == "[") && ((take 1 right) == "[") = compLists (l1,r1)
    | (take 1 left) == "[" = compLists (left,toList right)
    | (take 1 right) == "[" = compLists (toList left,right)
    | otherwise = case ((readFirstInt left),(readFirstInt right)) of
        ((Just x),Nothing) -> False
        (Nothing,(Just x)) -> True
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
        lscr = case (dropWhile (/=',') lrest) of
            "" -> ""
            xs -> tail xs
        rscr = case (dropWhile (/=',') rrest) of
            "" -> ""
            xs -> tail xs


oneDir :: String -> Pair
oneDir lines = case listInit of
    [] -> (lines,"")
    xs -> res
    where
        (beforeList,listInit) = span (/='[') lines
        firstList 
            | listInit /= "" = getDir (tail listInit) 1
            | otherwise = ""
        rest = drop ((length beforeList) + 1 + (length firstList)) listInit
        outerList = case rest of 
            "" -> beforeList
            ys -> case ys of
                (']':',':zs) | (last zs) == ']' -> concat [beforeList,(init zs)]
                (']':',':zs) -> concat [beforeList,zs]
                zs -> concat [beforeList,(init zs)]
        res 
            | (length (take 1 beforeList) == 1) && (isDigit (head beforeList)) = (beforeList,'[':listInit)
            | otherwise = (firstList, outerList)


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

sortPackets :: [String] -> [String]
sortPackets [] = []
sortPackets [x] = [x]
sortPackets (x:xs) = sortPackets [p | p <- xs, compLists (p,x)] ++ [x] ++  (sortPackets [p | p <- xs, not (compLists (x,p))])

getAllDirs :: String -> [String]
getAllDirs [] = []
getAllDirs str = case one of
    [] -> getAllDirs rest
    ys -> ys : getAllDirs rest
    where
        one 
            | str /= "" = getDir (tail str) 1
            | otherwise = ""
        tillcd = takeWhile (/='[') str
        rest = drop (1 + (length tillcd) + (length one)) $ tail str  

cmp :: Pair -> Bool
cmp ("","") = True
cmp ((x:xs),[]) = False
cmp ([],(x:xs)) = True
cmp (l@(x:xs),r@(y:ys)) = case (x,y) of
    (',',',') -> cmp (xs,ys)
--  (',',']') -> cmp (l,ys)
--  (']',',') -> cmp (xs,r)
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
--      (n1,n2) | n1 == n2 -> cmp (dropWhile (/=',') l, dropWhile (/=',') r)
        (n1,n2) | n1 == n2 -> cmp (dropWhile (not . ((flip elem) ",[]")) l, dropWhile (not . ((flip elem) ",[]")) r)
        (n1,n2) -> n2 >= n1
    where
        dig :: String -> String
        dig xs = takeWhile isDigit xs

        getDigListPass :: String -> String
        getDigListPass xs = '[':(dig xs) ++ "]" ++ rest
            where
                rest = drop (length $ dig xs) xs

        getDigPass :: String -> String
        getDigPass xs = (dig xs) ++ rest
            where
                rest = drop (length $ dig xs) xs

cmp' :: Pair -> [Pair]
cmp' ("","") = [("","")]
cmp' (l@(x:xs),[]) = [(l,"")]
cmp' ([],r@(x:xs)) = [("",r)]
cmp' (l@(x:xs),r@(y:ys)) = case (x,y) of
    (',',',') -> lr : (cmp' (xs,ys))
--  (',',']') -> lr : (cmp' (l,ys))
--  (']',',') -> lr : (cmp' (xs,r))
    (',',']') -> [lr]
    (']',',') -> [lr]
    ('[','[') -> lr : (cmp' (xs,ys))
    (']',']') -> lr : (cmp' (xs,ys))
    ('[',w) | isDigit w -> lr : (cmp' (l, getDigListPass r))
    (v,'[') | isDigit v -> lr : (cmp' (getDigListPass l, r))
    (']',w) | isDigit w -> [lr]
    (v,']') | isDigit v -> [lr]
    (v, w ) | (isDigit v) && (not (isDigit w)) -> [lr]
    (v, w ) | (not (isDigit v)) && (isDigit w) -> [lr]
    ('[',']') -> [lr]
    (']','[') -> [lr]
    (v, w ) | (isDigit v) && (isDigit w) -> case (read (dig l) :: Int, read (dig r) :: Int) of
--      (n1,n2) | n1 == n2 -> lr : (cmp' (dropWhile (/=',') l, dropWhile (/=',') r))
        (n1,n2) | n1 == n2 -> lr : (cmp' (dropWhile (not . ((flip elem) ",[]")) l, dropWhile (not . ((flip elem) ",[]")) r))     
        (n1,n2) -> [lr]
    where
        lr = (l,r)
        dig :: String -> String
        dig xs = takeWhile isDigit xs

        getDigListPass :: String -> String
        getDigListPass xs = '[':(dig xs) ++ "]" ++ rest
            where
                rest = drop (length $ dig xs) xs

        getDigPass :: String -> String
        getDigPass xs = (dig xs) ++ rest
            where
                rest = drop (length $ dig xs) xs

srt :: [String] -> [String]
srt [] = []
srt [x] = [x]
srt (x:xs) = srt [p | p <- xs, cmp (p,x)] ++ [x] ++  (srt [p | p <- xs, not (cmp (p,x))])
