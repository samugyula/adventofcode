module Read2Darr
( readArr
, ElemC
, Coords
) where

type Coords = (Int,Int)
type ElemC = (Coords,Char)

readArr :: [String] -> [ElemC]
readArr lines = readArr' 1 lines
    where
        nLine = length $ head lines
        readArr' :: Int -> [String] -> [ElemC]
        readArr' _ [] = []
        readArr' n (x:xs) = [((n,m),light) | (m,light) <- zip [1..nLine] x] ++ readArr' (n+1) xs

