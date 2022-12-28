import System.IO
import System.IO.Unsafe

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        p = map (maximum . nGvals . (paths 24 (0,1,0,0,0,0,0,0))) (readCosts fileLines)
        res1 = sum [n*x | (n,x) <- zip [1..] p]
        res2 = [maximum [max' fileLines n | n <- [1,2,3]]]
    putStr $ result [res1, product ( concat res2)]

result :: Show a => [a] -> String
result xs = unlines $ map show xs

max' :: [String] -> Int -> [Int]
max' fileLines n = newMaxes 
    where
        c = (readCosts fileLines) !! n
        p = paths 24 (0,1,0,0,0,0,0,0) c
        m6 = (maximum $ vals 6 $ p)
        m4 = (maximum $ vals 4 $ p)
        maxStore
            | m6 /= 0 = filter (\(_,_,_,_,_,_,x,_) -> x > (div m6 2)) $ stores p
            | otherwise = filter (\(_,_,_,_,x,_,_,_) -> x > (div m4 2)) $ stores p
        newTrees = [paths' 8 x c | x <- maxStore]
        newMaxes = [maximum $ vals 6 $ pp | pp <- newTrees]

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

paths' :: Int -> Store -> Costs -> Tree Store
paths' 0 st _ = Leaf st
paths' n s@(nO,nOR,nC,nCR,nOb,nObR,nG,nGR) costs@(cO,cC,cOb1,cOb2,cG1,cG2)
        | length valid == 0 = paths' (n-1) (nO + nOR, nOR, nC + nCR, nCR, nOb + nObR, nObR, nG + nGR, nGR) costs
        | otherwise = Node [ p n st s costs | (p,st) <- zip [p1',p2',p3',p4'] starr, st <= n]
    where
        steps1 = n+1
        steps2 = n+1
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

        p1' n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = 
            paths' (n - s) (nO + nOR * s - cO, nOR + 1, nC + nCR * s, nCR, nOb + nObR * s, nObR, nG + nGR * s, nGR)
        p2' n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = 
            paths' (n - s) (nO + nOR * s - cC, nOR, nC + nCR * s, nCR + 1, nOb + nObR * s, nObR, nG + nGR * s, nGR)
        p3' n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = 
            paths' (n - s) (nO + nOR * s - cOb1, nOR, nC + nCR * s - cOb2, nCR, nOb + nObR * s, nObR + 1, nG + nGR * s, nGR)
        p4' n s (nO,nOR,nC,nCR,nOb,nObR,nG,nGR) = 
            paths' (n - s) (nO + nOR * s - cG1, nOR, nC + nCR * s, nCR, nOb + nObR * s - cG2, nObR, nG + nGR * s, nGR + 1)

nGvals :: Tree Store -> [Int]
nGvals (Leaf x@(nO,nOR,nC,nCR,nOb,nObR,nG,nGR)) = [nG]
nGvals (Node xs) = concat [nGvals x | x <- xs]

stores :: Tree Store -> [Store]
stores (Leaf x) = [x]
stores (Node xs) = concat [stores x | x <- xs]

vals :: Int -> Tree Store -> [Int]
vals n (Leaf x@(nO,nOR,nC,nCR,nOb,nObR,nG,nGR)) = [xs !! n]
    where
        xs = [nO,nOR,nC,nCR,nOb,nObR,nG,nGR]
vals n (Node xs) = concat [vals n x | x <- xs]
