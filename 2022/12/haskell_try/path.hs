import Data.List
import Data.Maybe (catMaybes)
import Read2Darr
import System.IO.Unsafe
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    hSetBuffering stdout NoBuffering
    contents <- hGetContents handle
    let
        fileLines = lines contents

        grid = readArr fileLines
        trgrid = transform grid

--      start' = head $ dropWhile (\((_,_),c) -> c /= 'S') grid
--      end' = head $ dropWhile (\((_,_),c) -> c /= 'E') grid

--      trfSE = transform [start',end']
--      start = head trfSE
--      end = last trfSE

--      paths = getPaths trgrid end start [] 0
--      minL = getMinLeaf paths
--      p = getLeafs paths

--  putStr $ show p
--  putStr $ result p--[minL]
--  putStr $ result [length p]
--  putStr $ result [length (nub p)]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents

grid = readArr fileLines
trgrid = transform grid

start' = head $ dropWhile (\((_,_),c) -> c /= 'S') grid
end' = head $ dropWhile (\((_,_),c) -> c /= 'E') grid

trfSE = transform [start',end']
--start@((s1,s2),c) = head trfSE
--end = last trfSE

getNeighbors :: [Elem] -> Coords -> [Elem]
getNeighbors list (x,y) = res ids
    where
        ((mx,my),_) = last list
        start 
            | x > 1 = (x-2)*my + y - 1
            | otherwise = case y of 
                1 -> y - 1
                _ -> y - 2
        lines = drop start (take (x*my + y) list)

        iTop 
            | x > 1 = Just $ (x-2)*my + y - 1
            | otherwise = Nothing
        iBottom
            | x < mx = Just $ x*my + y - 1                                                                                       
            | otherwise = Nothing
        iLeft 
            | y > 1 = Just $ (x-1)*my + y - 2
            | otherwise = Nothing
        iRight 
            | y < my = Just $ (x-1)*my + y
            | otherwise = Nothing

        ids = catMaybes [iTop,iBottom,iLeft,iRight]
        res :: [Int] -> [Elem]  
        res [] = []
        res (x:xs) = case (take 1 (drop (x-start) lines)) of
            [] -> res xs
            [y] -> y : res xs

ind :: Char -> Int
ind c = case(elemIndex c ['a'..'z']) of
    Just x -> x
    Nothing -> case c of
        'S' -> 0
        'E' -> (length ['a'..'z']) - 1

type Elem = (Coords,Int)

transform :: [ElemC] -> [Elem]
transform [] = []
transform ((coord,c):xs) = (coord,ind c) : transform xs

getGoodNeighbors :: [Elem] -> Elem -> [Elem]
getGoodNeighbors list (coords,c) = filter (\(_,cc) -> (cc + 1) >= c) $ getNeighbors list coords
{-
getGoodNeighbors list (coords,c) = filter (\(_,cc) -> ((ind cc) + 1) >= (ind c)) $ getNeighbors list coords
    where
        ind :: Char -> Int
        ind c = case(elemIndex c ['a'..'z']) of
            Just x -> x
            Nothing -> case c of
                'S' -> 0
                'E' -> (length ['a'..'z']) - 1
-}

data Tree b = Leaf b | Node [Maybe (Tree b)] deriving (Show)

getPaths' :: [Elem] -> Elem -> Elem -> [Elem] -> Int -> Maybe (Tree [Elem])
getPaths' list item start@((s1,s2),_) already n
    | item == start = Just (Leaf new)
    | item `elem` already = Nothing
    | otherwise = Just $ Node [getPaths' list x start new (n+1) | x <- ns]
    where
        new = item : already
        ns' = getGoodNeighbors list item
        difs = sort $ [(((s1-x)^2 + (s2-y)^2),xy) | xy@((x,y),_) <- ns' ]
        ns = [ e | (d,e) <- difs]

getPaths :: [Elem] -> Elem -> Elem -> [Elem] -> Int -> Maybe (Tree Int)
getPaths list item start@((s1,s2),_) already n
    | item == start = Just (Leaf n)
    | item `elem` already = Nothing
    | otherwise = Just $ Node [getPaths list x start new (n+1) | x <- ns]
    where
        new = item : already
        ns' = getGoodNeighbors list item
        difs = sort $ [(((s1-x)^2 + (s2-y)^2),xy) | xy@((x,y),_) <- ns' ]
        ns = [ e | (d,e) <- difs]

getLeafs' :: Maybe (Tree [Elem]) -> [[Elem]]
getLeafs' Nothing = []
getLeafs' (Just (Leaf x)) = [x]
getLeafs' (Just (Node xs)) = concat (map getLeafs' xs)

getLeafs :: Maybe (Tree Int) -> [Int]
getLeafs Nothing = []
getLeafs (Just (Leaf x)) = [x]
getLeafs (Just (Node xs)) = concat (map getLeafs xs)

getMinLeaf :: Maybe (Tree Int) -> Int
getMinLeaf = minimum . getLeafs

coordtrf :: Coords -> Coords -> Int
coordtrf (x,y) (mx,my) = (x-1)*my + y

extract :: [Elem] -> Coords -> Coords -> [Elem]
extract list c1@(x1,y1) c2@(x2,y2) = [item | item@((i1,i2),_) <- list, x1 <= i1, i1 <=  x2, y1 <= i2, i2 <= y2]

prob = extract trgrid (1,1) (4,4)
path = getPaths' prob (last prob) (head prob) [] 0
l = getLeafs' path

nNodes = mx*my
    where
        ((mx,my),_) = last trgrid

adjMat = replicate nNodes $ replicate nNodes 0
