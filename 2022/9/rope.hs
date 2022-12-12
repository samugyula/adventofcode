import System.IO.Unsafe
import Data.List
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents

        moves = getMoves fileLines

        eval = evalThem [((0,0),(0,0))] moves
        tailsteps = map snd eval
        res = length $ nub tailsteps

        first2 = replicate 10 (0,0)
        eval2 = evalThemKnots [first2] moves
        tailsteps2 = map last eval2
        res2 = length $ nub tailsteps2

    putStr $ result [res,res2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

type Move = (Char,Int)
type Moves = [(Char,Int)]

type Pos = (Int,Int)
type PPos = (Pos,Pos)

getMoves :: [String] -> Moves
getMoves [] = []
getMoves (x:xs) = case (words x) of
    (c:n:[]) -> (head c, read n :: Int) : getMoves xs

headEval :: Pos -> Char -> Pos
headEval h@(xh,yh) d = case d of
    'R' -> (xh + 1, yh)
    'L' -> (xh - 1, yh)
    'U' -> (xh, yh + 1)
    'D' -> (xh, yh - 1)

tailEval :: Pos -> Pos -> Pos
tailEval hh@(xhh,yhh) tt@(xtt,ytt)
    | (xtt >= (xhh - 1)) && (xtt <= (xhh + 1)) && (ytt >= (yhh - 1)) && (ytt <= (yhh + 1)) = getNew 0 0
    | (xtt < (xhh - 1)) && (ytt == yhh) = getNew 1 0
    | (xtt > (xhh + 1)) && (ytt == yhh) = getNew (-1) 0
    | (ytt < (yhh - 1)) && (xtt == xhh) = getNew 0 1
    | (ytt > (yhh + 1)) && (xtt == xhh) = getNew 0 (-1)

    | (xtt < (xhh - 1)) && (ytt < yhh) =  getNew 1 1
    | (xtt > (xhh + 1)) && (ytt < yhh) =  getNew (-1) 1
    | (ytt < (yhh - 1)) && (xtt < xhh) =  getNew 1 1
    | (ytt > (yhh + 1)) && (xtt < xhh) =  getNew 1 (-1)

    | (xtt < (xhh - 1)) && (ytt > yhh) =  getNew 1 (-1)
    | (xtt > (xhh + 1)) && (ytt > yhh) =  getNew (-1) (-1)
    | (ytt < (yhh - 1)) && (xtt > xhh) =  getNew (-1) 1
    | (ytt > (yhh + 1)) && (xtt > xhh) =  getNew (-1) (-1)
    where
        getNew :: Int -> Int -> Pos
        getNew x y = (xtt+x,ytt+y)

moveEval :: Pos -> Pos -> Move -> [PPos]
moveEval t h (d,n) = moveEval' t h 0
    where 
        moveEval' :: Pos -> Pos -> Int -> [PPos]
        moveEval' tt@(xtt,ytt) hh nn 
            | nn == n = [(hh,tt)]
            | otherwise = (hh,tt) : getNew
            where
                nhh = headEval hh d
                getNew = moveEval' (tailEval nhh tt) nhh (nn+1)

evalThem :: [PPos] -> Moves -> [PPos]
evalThem ps [] = ps
evalThem ps (s:ss) = newPPos ++ (tail (evalThem newPPos ss))
    where
        (h,t) = last ps
        newPPos = moveEval t h s

stepEvalKnots :: Pos -> [Pos] -> [Pos]
stepEvalKnots xh [] = []
stepEvalKnots xh (xt:xs) = newT : stepEvalKnots newT xs
    where
        newT = tailEval xh xt

moveEvalKnots :: [Pos] -> Move -> [[Pos]]
moveEvalKnots l (d,n) = moveEvalKnots' l 0
    where
        moveEvalKnots' x@(xh:xs) nn
            | n == nn = []
            | otherwise = newL : moveEvalKnots' newL (nn+1)
            where
                newH = headEval xh d
                newL = newH : stepEvalKnots newH xs

evalThemKnots :: [[Pos]] -> Moves -> [[Pos]]
evalThemKnots ps [] = ps
evalThemKnots ps (s:ss) = newPPos ++ (evalThemKnots newPPos ss)
    where
        newPPos = moveEvalKnots (last ps) s
