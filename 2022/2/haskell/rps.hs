import Data.List
import System.IO

main = do  
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let 
        fileLines = lines contents
        sumPoints = sum $ map resFromLine fileLines
        sumPoints2 = sum $ map resFromResFromLine fileLines
    putStr $ show sumPoints ++ "\n" ++ show sumPoints2 ++ "\n"
    hClose handle

dict = zip ['A'..'C'] ['X'..'Z']
syms = "ABC"
ptdict = zip ['A'..'C'] [1..]

pl1 :: String -> String
pl1 str = (words str) !! 0 

getVal :: Eq b => [(a,b)] -> b -> a
getVal ((val,key):xs) x
    | x == key  = val
    | otherwise = getVal xs x

pl2 :: String -> String
pl2 str = getVal dict (((words str) !! 1) !! 0) : []

getSyms :: String -> (Char, Char)
getSyms str = (((pl1 str) !! 0), ((pl2 str) !! 0))

point :: Char -> Int
point sym = [p | (s,p) <- ptdict, s == sym] !! 0

res :: Char -> Char -> Int
res s1 s2 = case (length scr) of
    1 -> 0 + pt
    2 -> 6 + pt
    0 -> 3 + pt
    where
        scr = takeWhile (/= s1) $ dropWhile (/= s2) $ cycle syms
        pt = point s2

resFromLine :: String -> Int
resFromLine str = case (getSyms str) of
    (p1,p2) -> res p1 p2

resdict = zip [0,3,6] ['X'..'Z']

resFromRes s1 sres = [ res s1 s2 | s2 <- syms, ((getVal resdict sres) + (point s2)) == (res s1 s2)] !! 0

resFromResFromLine str = case (words str) of
    (p1:sres:[]) -> resFromRes (p1 !! 0) (sres !! 0)
