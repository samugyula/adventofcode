import Data.Char
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        res1 = addNums contents
        res2 = addNums $ reduceRed contents
    putStr $ result [res1,res2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

getNum :: String -> Maybe (Int,String)
getNum str  = case (getNumStr str) of
    ("","") -> Nothing
    (x1,x2) -> Just (read x1 :: Int, x2)

getNumStr :: String -> (String,String)
getNumStr "" = ("","")
getNumStr (x:y:xs) 
    | x == '-' && isDigit y = (x : (fst (getNumStr (y:xs))), (snd (getNumStr (y:xs))))
    | isDigit x && (not . isDigit) y = ([x],(y:xs))
getNumStr (x:xs) 
    | isDigit x = (x : (fst (getNumStr xs)), (snd (getNumStr xs)))
    | otherwise = getNumStr xs

addNums :: String -> Int
addNums str = case (getNum str) of
    (Just (x,rest)) -> x + addNums rest
    Nothing -> 0

showNum :: Maybe (Int,String) -> Int
showNum (Just (x,str)) = x

containsRed :: String -> Bool
containsRed (x:y:"") = False
containsRed (x:y:z:xs) = case (x:y:z:"") of
    "red" -> True
    other -> containsRed (y:z:xs)

getBeforeRed :: String -> String
getBeforeRed (x:y:"") = x:y:""
getBeforeRed (x:y:z:xs) = case (x:y:z:"") of
    "red" -> ""
    other -> x : getBeforeRed (y:z:xs)

getAfterRed :: String -> String
getAfterRed (x:y:"") = x:y:""
getAfterRed (x:y:z:xs) = case (x:y:z:"") of
    "red" -> xs
    other -> getAfterRed (y:z:xs)

getAfterBracket :: String -> String
getAfterBracket str
    | length scr1 < length scr2 = drop ((length scr1)-1) str
    | otherwise = str
    where
        scr1 = firstUnpairedCurly  "forward" str 0
        scr2 = firstUnpairedSquare "forward" str 0

getBeforeBracket :: String -> String
getBeforeBracket str 
    | length scr1 < length scr2 = reverse (drop ((length scr1)-1) (reverse str))
    | otherwise = str
    where
        scr1 = firstUnpairedCurly  "backward" (reverse str) 0
        scr2 = firstUnpairedSquare "backward" (reverse str) 0

firstUnpairedCurly :: String -> String -> Int -> String
firstUnpairedCurly "forward"  xs   1  = ""
firstUnpairedCurly "backward" xs (-1) = ""
firstUnpairedCurly dir ('}':xs) n = '}':firstUnpairedCurly dir xs (n+1)
firstUnpairedCurly dir ('{':xs) n = '{':firstUnpairedCurly dir xs (n-1)
firstUnpairedCurly dir (x:xs)   n = x : firstUnpairedCurly dir xs n
firstUnpairedCurly "forward"  "" n = ""
firstUnpairedCurly "backward" "" n = ""

firstUnpairedSquare :: String -> String -> Int -> String
firstUnpairedSquare "forward"  xs   1  = ""
firstUnpairedSquare "backward" xs (-1) = ""
firstUnpairedSquare dir (']':xs) n = ']':firstUnpairedSquare dir xs (n+1)
firstUnpairedSquare dir ('[':xs) n = '[':firstUnpairedSquare dir xs (n-1)
firstUnpairedSquare dir (x:xs)   n = x : firstUnpairedSquare dir xs n
firstUnpairedSquare "forward"  "" n = ""
firstUnpairedSquare "backward" "" n = ""

reduceRed :: String -> String
reduceRed str = case (containsRed str) of
    True -> reduceRed ( getBeforeBracket (getBeforeRed str) ++ ( getAfterBracket (getAfterRed str)))
    False -> str
