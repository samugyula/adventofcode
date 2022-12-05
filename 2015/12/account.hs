import Data.Char
import System.IO.Unsafe

contents = unsafePerformIO . readFile $ "data.txt"

--fileLines = lines contents

getNum :: String -> Maybe (Int,String)
getNum str  = case (getNumStr str) of
    ("","") -> Nothing
    (x1,x2) -> Just (read x1 :: Int, x2)

getNumStr :: String -> (String,String)
getNumStr "" = ("","")
getNumStr (x:y:xs) 
    | x == '-' && isDigit y = (x : y : (fst (getNumStr xs)), (snd (getNumStr xs)))
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
