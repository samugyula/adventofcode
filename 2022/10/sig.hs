import System.IO.Unsafe
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        steps = getVals fileLines
        lns = unlines $ [ take 40 (drop ((n-1)*40) ( makeStr steps 1)) | n <- [1..6]]
    putStr $ lns
    hClose handle

type Step = (String,Int)
type Steps = [Step]

getVals :: [String] -> Steps
getVals [] = []
getVals (x:xs) = case (words x) of
    (y:[]) -> (y,0 :: Int) : getVals xs
    (y:z:[])  -> (y, read z :: Int) : getVals xs

getStep :: Step -> Int
getStep (x1,x2) 
    | x1 == "noop" = 1
    | x1 == "addx" = 2

sigAtTime :: Steps -> Int -> Int
sigAtTime l t = countSig l 0 1
    where
        countSig :: Steps -> Int -> Int -> Int
        countSig [] tt ss = t*ss
        countSig (x@(x1,n1):xs) tt ss
            | tt + (getStep x) < t = countSig xs (tt + (getStep x)) (ss + n1)
            | otherwise = t*ss

regAtTime :: Steps -> Int -> Int
regAtTime l t = countSig l 0 1
    where
        countSig :: Steps -> Int -> Int -> Int
        countSig [] tt ss = ss
        countSig (x@(x1,n1):xs) tt ss
            | tt + (getStep x) < t = countSig xs (tt + (getStep x)) (ss + n1)
            | otherwise = ss
            

makeStr :: Steps -> Int -> String
makeStr _ 241 = ""
makeStr steps t 
    | (reg >= ((mod (t-1) 40) - 1)) && (reg <= ((mod (t-1) 40) + 1)) = '#' : makeStr steps (t+1)
    | otherwise = '.' : makeStr steps (t+1)
    where
        reg = regAtTime steps t

