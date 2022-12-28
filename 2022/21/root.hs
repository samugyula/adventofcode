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
        monkeys = readMonkeys fileLines
        res1 = head [ (name, head v) | (name,v) <- (fullReduce monkeys), name == "root" ]
        indep = indepVals monkeys (dependence monkeys "humn") 
        re = reWrite monkeys
        res2 = fullBackTrack re "root" "0" indep

    putStr $ result [res1, res2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


contents = unsafePerformIO . readFile $ "data.txt"

type Monkey = (String,[String])
type Monkeys = [Monkey]

readMonkeys :: [String] -> Monkeys
readMonkeys [] = []
readMonkeys (line:lines) = (name,str) : readMonkeys lines
    where
        l = words line
        name = init $ head l
        str = tail l

eval :: [String] -> [String]
eval (x:op:y:[]) = [show $ f (read x :: Int) (read y :: Int)]
    where
        f = case op of
            "+" -> (+)
            "-" -> (-)
            "*" -> (*)
            "/" -> div

reduce :: Monkey -> Monkeys -> Monkey
reduce (name,wrds) ms = (name,x)
    where
        found = [ (name',v) | (name',v) <- ms, name' `elem` wrds, (length v) == 1]

        check :: Int -> String
        check n = str'
            where
                orig = wrds !! n
                scr = [ head val' | (name',val') <- found, name' == orig ]
                str'
                    | length scr /= 0 = head scr
                    | otherwise = orig
        
        newFirst = check 0
        newSecond = check 2
        op = wrds !! 1
        newStr = [newFirst,op,newSecond]

        x
            | (isInt newFirst) && (isInt newSecond) = eval newStr
            | otherwise = newStr

isInt :: String -> Bool
isInt str = case (readMaybe str :: Maybe Int) of
    Nothing -> False
    _ -> True


reduceAll :: Monkeys -> Monkeys -> Monkeys
reduceAll [] _ = []
reduceAll (m@(_,str):ms) orig = case str of
    x | length x == 1 -> m : reduceAll ms orig
    _ -> (reduce m orig) : reduceAll ms orig

fullReduce :: Monkeys -> Monkeys
fullReduce ms = case (reduceAll ms ms) of
    x | x == ms -> x
    x -> fullReduce x

indepVals :: Monkeys -> [String] -> Monkeys
indepVals ms dep = [ m | m@(name,_) <- (fullReduce ms), not $ name `elem` dep ]

dependence :: Monkeys -> String -> [String]
dependence ms name 
    | length uses == 0 = [name]
    | otherwise = name : (dependence ms $ head uses)
    where
        uses = [ n | (n,str) <- ms, name `elem` str]

reWrite :: Monkeys -> Monkeys
reWrite (m@("root",(x:op:y:[])):ms) = ("root",(x:"-":y:[])) : ms
reWrite (m:ms) = m : reWrite ms

invOp :: String -> String
invOp op = case op of
    "+" -> "-"
    "-" -> "+"
    "*" -> "/"
    "/" -> "*"

backTrack :: Monkeys -> String -> String -> Monkeys -> (String,String) 
backTrack ms name val indep = (newName, newVal)
    where
        str = head [ v | m@(n,v) <- ms, n == name ]
        (indepName,indepVal) = head [ (n, head v) | (n,v) <- indep, n `elem` str ]
        nIndep = case (elemIndex indepName str) of
            Just x -> x
        newName = head [ n | n <- [str !! 0, str !! 2], n /= indepName ]
        op = (str !! 1)
        newVal 
            | (nIndep == 0) && (op `elem` ["/","-"]) = head $ eval [indepVal, op, val]
            | otherwise = head $ eval [val, invOp op, indepVal]

fullBackTrack :: Monkeys -> String -> String -> Monkeys -> (String,String)
fullBackTrack ms name val indep = case (backTrack ms name val indep) of
    res@("humn",_) -> res
    res@(n,v) -> fullBackTrack ms n v indep
