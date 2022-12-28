import System.IO
import System.IO.Unsafe

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
--      p = map (maximum . max' . (paths 24 (0,1,0,0,0,0,0,0))) (readCosts fileLines)
        p = map (maximum . max' . (paths 32 (0,1,0,0,0,0,0,0))) (take 3 $ readCosts fileLines)
--  putStr $ result [sum [n*x | (n,x) <- zip [1..] p]]
    putStr $ result [product p]

result :: Show a => [a] -> String
result xs = unlines $ map show xs


contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents

readCosts :: [String] -> [Costs]
readCosts [] = []
readCosts (l:ls) = tup : readCosts ls
    where
        xs = words l
        list@(c1:c2:c3:c4:c5:c6:[]) = [read x :: Int | x <- [xs !! 6, xs !! 12, xs !! 18, xs !! 21, xs !! 27, xs !! 30]]
        tup = (c1,c2,c3,c4,c5,c6)

data Tree a = Leaf a | Node [Tree a] deriving (Show)

type Store = (Int,Int,Int,Int,Int,Int,Int,Int)
type Costs = (Int,Int,Int,Int,Int,Int)

one (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = (nO+nOR,nOR,nC+nCR,nCR,nOb + nObR,nObR,nG + nGR,nGR)

paths :: Int -> Store -> Costs -> Tree Store
paths 0 st _ = Leaf st
paths n s@(nO,nOR,nC,nCR,nOb,nObR,nG,nGR) costs@(cO,cC,cOb1,cOb2,cG1,cG2)
        | length valid == 0 = paths (n-1) (nO + nOR, nOR, nC + nCR, nCR, nOb + nObR, nObR, nG + nGR, nGR) costs
        | otherwise = Node [ p n st s costs | (p,st) <- zip [p1,p2,p3,p4] starr, st <= n]
    where
        steps1 = step1 s 1
        steps2 = step2 s 1
        steps3 
            | nCR > 0 = step3 s 1
            | otherwise = n + 1
        steps4 
            | nObR > 0 = step4 s 1
            | otherwise = n + 1
        starr = [steps1,steps2,steps3,steps4]
        valid = filter (n >=) starr

        step1 s@(nO,_,_,_,_,_,_,_) n
            | nO < cO = step1 (one s) (n+1)
            | otherwise = n

        step2 s@(nO,_,_,_,_,_,_,_) n
            | nO < cC = step2 (one s) (n+1)
            | otherwise = n

        step3 s@(nO,_,nC,_,_,_,_,_) n
            | nO < cOb1 || nC < cOb2 = step3 (one s) (n+1)
            | otherwise = n

        step4 s@(nO,_,_,_,nOb,_,_,_) n
            | nO < cG1 || nOb < cG2 = step4 (one s) (n+1)
            | otherwise = n

        p1 n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = 
            paths (n - s) (nO + nOR * s - cO, nOR + 1, nC + nCR * s, nCR, nOb + nObR * s, nObR, nG + nGR * s, nGR)
        p2 n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = 
            paths (n - s) (nO + nOR * s - cC, nOR, nC + nCR * s, nCR + 1, nOb + nObR * s, nObR, nG + nGR * s, nGR)
        p3 n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = 
            paths (n - s) (nO + nOR * s - cOb1, nOR, nC + nCR * s - cOb2, nCR, nOb + nObR * s, nObR + 1, nG + nGR * s, nGR)
        p4 n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = 
            paths (n - s) (nO + nOR * s - cG1, nOR, nC + nCR * s, nCR, nOb + nObR * s - cG2, nObR, nG + nGR * s, nGR + 1)

p1' n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) coll = 
    paths' (n - s) one (coll ++ [((n-s),one)])
    where
        one = (nO + nOR * s - 4, nOR + 1, nC + nCR * s, nCR, nOb + nObR * s, nObR, nG + nGR * s, nGR)
p2' n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) coll = 
    paths' (n - s) one (coll ++ [((n-s),one)])
    where
        one = (nO + nOR * s - 2, nOR, nC + nCR * s, nCR + 1, nOb + nObR * s, nObR, nG + nGR * s, nGR)
p3' n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) coll = 
    paths' (n - s) one (coll ++ [((n-s),one)])
    where
        one = (nO + nOR * s - 3, nOR, nC + nCR * s - 14, nCR, nOb + nObR * s, nObR + 1, nG + nGR * s, nGR)
p4' n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) coll = 
    paths' (n - s) one (coll ++ [((n-s),one)])
    where
        one = (nO + nOR * s - 2, nOR, nC + nCR * s, nCR, nOb + nObR * s - 7, nObR, nG + nGR * s, nGR + 1)

paths' :: Int -> Store -> [(Int,Store)] -> Tree [(Int,Store)]
paths' 0 st coll = Leaf coll
paths' n s@(nO,nOR,nC,nCR,nOb,nObR,nG,nGR) coll
        | length valid == 0 = paths' (n-1) one' (coll ++ [((n-1),one')])
        | otherwise = Node [ p n st s coll | (p,st) <- zip [p1',p2',p3',p4'] starr, st <= n]
    where
        steps1 = step1 s 1
        steps2 = step2 s 1
        steps3 
            | nCR > 0 = step3 s 1
            | otherwise = n + 1
        steps4 
            | nObR > 0 = step4 s 1
            | otherwise = n + 1
        starr = [steps1,steps2,steps3,steps4]
        valid = filter (n >=) starr
        one' = (nO + nOR, nOR, nC + nCR, nCR, nOb + nObR, nObR, nG + nGR, nGR)

        step1 s@(nO,_,_,_,_,_,_,_) n
            | nO < 4 = step1 (one s) (n+1)
            | otherwise = n

        step2 s@(nO,_,_,_,_,_,_,_) n
            | nO < 2 = step2 (one s) (n+1)
            | otherwise = n

        step3 s@(nO,_,nC,_,_,_,_,_) n
            | nO < 3 || nC < 14 = step3 (one s) (n+1)
            | otherwise = n

        step4 s@(nO,_,_,_,nOb,_,_,_) n
            | nO < 2 || nOb < 7 = step4 (one s) (n+1)
            | otherwise = n

numLeafs :: Tree Store -> Int
numLeafs (Leaf x) = 1
numLeafs (Node xs) = sum [numLeafs x | x <- xs]

max' :: Tree Store -> [Int]
max' (Leaf x@(nO,nOR,nC,nCR,nOb,nObR,nG,nGR)) = [nG]
max' (Node xs) = concat [max' x | x <- xs]
