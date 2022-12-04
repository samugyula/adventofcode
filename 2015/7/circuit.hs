import System.IO
import Data.Bits
import Data.Word
import Data.Char
import Text.Read

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        dict = map parseLine fileLines
        reducedDict = reduceAll dict 0
        signal1 = (getVal "a" reducedDict) !! 0
        newDict = rewrite dict signal1
        reducedNewDict = reduceAll newDict 0
        signal2 = (getVal "a" reducedNewDict) !! 0
    putStr $ signal1 ++ "\n" ++ signal2 ++ "\n"

type Database = [([String],String)]

rewrite :: Database -> String -> Database
rewrite ((_,"b"):xs) new = ([new],"b"):xs
rewrite (x:xs)       new = x : rewrite xs new

reduce :: Database -> [String] -> [String]
reduce dict [] = []
reduce dict (x:xs) = case (readMaybe x :: Maybe Word16) of
    Just val -> x : (reduce dict xs)
    Nothing | (all isUpper x) -> x : (reduce dict xs)
    Nothing | length (getVal x dict) == 1 -> ((getVal x dict) !! 0) : (reduce dict xs)
    Nothing -> x:(reduce dict xs)

eval :: [String] -> [String]
eval xs = case xs of
    (x:[]) -> [x]
    ("NOT":x:[]) -> case (readMaybe x :: Maybe Word16) of
                    Just val -> [show (complement val)]
                    Nothing  -> xs
    (x:y:z:[]) -> case (readMaybe x :: Maybe Word16) of
                    Just val -> case (readMaybe z :: Maybe Word16) of
                                  Just val2 -> case y of
                                                "AND"    -> [show (val .&. val2)]
                                                "OR"     -> [show (val .|. val2)]
                                                "RSHIFT" -> [show (shiftR val (fromIntegral val2 :: Int))]
                                                "LSHIFT" -> [show (shiftL val (fromIntegral val2 :: Int))]
                                  Nothing -> xs
                    Nothing -> xs

trf :: Database -> Database -> Int -> (Database,Int)
trf [] _  n = ([],n)
trf ((list,str):xs) dict n = case (eval (reduce dict list)) of
    x | x == list -> ((x,str):res,nn)
    x | x /= list -> ((x,str):res,1)
    where 
        (res,nn) = trf xs dict n

reduceAll :: Database -> Int -> Database
reduceAll dict n = case (trf dict dict n) of
    (x,1) -> reduceAll x 0
    (x,0) -> x


parseLine :: String -> ([String], String)
parseLine line = case (reverse (words line)) of
    (x:y:xs) -> (reverse xs,x)

getVal :: String -> [([String],String)] -> [String]
getVal x [] = error $ "Not found: " ++ x
getVal x ((val,key):xs)
    | x == key = val
    | otherwise = getVal x xs

