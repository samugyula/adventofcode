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

compLists :: Pair -> Bool
compLists ("","") = True
{-
compLists (_,"") = False
compLists ("",_) = True
-}
compLists (left,right)
    | ((take 1 left) == "[") && ((take 1 right) == "[") = compLists (l1,r1)
    | (take 1 left) == "[" = compLists (l1,right)
    | (take 1 right) == "[" = compLists (left,r1)
--  | (not (elem '[' left)) && (not (elem '[' right)) && ((length (filter (==',') left)) == (length (filter (==',') right)))
    | l1 == r1 = compLists (lrest,rrest)
    | otherwise = case ((readFirstInt left),(readFirstInt right)) of
        ((Just x),Nothing) -> False
        (Nothing,(Just x)) -> True
--      (Nothing,Nothing) -> compLists (tail (dropWhile (/=',') lrest),tail (dropWhile (/=',') rrest))
        (Nothing,Nothing) -> case (lscr,rscr) of
            ("","") -> True --compLists (lrest,rrest)
            (_,"") -> False --compLists (lscr,rrest)
            ("",_) -> True --compLists (lrest,rscr)
            (_,_) -> compLists (lscr,rscr)
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
    xs -> (firstList,outerList)
    where
        (beforeList,listInit) = span (/='[') lines
        firstList = getDir (tail listInit) 1
        rest = drop ((length beforeList) + 1 + (length firstList)) listInit
        outerList' = case rest of 
            "" -> beforeList
            ys -> concat [beforeList,"[",ys]
        outerList = case outerList' of
            '[':']':[] -> "" 
            '[':']':ys -> ys
            ys -> ys
{-
        rest
            | rest' == "" = ""
            | (last rest') /= "" = rest ++ "]"
            | otherwise = rest
-}

--stripDir :: String -> String


getDir :: String -> Int -> String
getDir [] n = []
getDir (x:xs) 0 = []
getDir (x:xs) n = case x of
    ']' | n == 1 -> getDir xs (n-1)
    ']' -> x : getDir xs (n-1)
    '[' -> x : getDir xs (n+1)
    y -> x : getDir xs n

