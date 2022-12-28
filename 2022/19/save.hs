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
