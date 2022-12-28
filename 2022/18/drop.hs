import System.IO.Unsafe
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    hSetBuffering stdout NoBuffering
    let
        fileLines = lines contents

        cubes = readCoords fileLines

        res = countSides cubes []
        emptyCells = empty cubes

        surroundCells = walk emptyCells [minimum emptyCells]

        innerCells = [x | x <- emptyCells, not (x `elem` surroundCells)]

        bubbles = getBubbles innerCells

        innerSurfaces = map (flip countSides []) bubbles

        chunk1 = walk innerCells [head innerCells]

    putStr $ result [res,res - (sum innerSurfaces) ]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

type Coords = (Int,Int,Int)

rInts :: String -> [Int]
rInts [] = []
rInts xs = (read (takeWhile (/=',') xs) :: Int) : rInts ( dropWhile (==',') $ dropWhile (/=',') xs )

readCoords :: [String] -> [Coords]
readCoords [] = []
readCoords (c:cs) = case (rInts c) of
    (x:y:z:[]) -> (x,y,z) : readCoords cs

neighbor :: Coords -> Coords -> Bool
neighbor (x1,y1,z1) (x2,y2,z2) = 
    ((x1 == x2) && (y1 == y2) && (((z1+1) == z2) || ((z1-1) == z2)))
 || ((y1 == y2) && (z1 == z2) && (((x1+1) == x2) || ((x1-1) == x2)))
 || ((z1 == z2) && (x1 == x2) && (((y1+1) == y2) || ((y1-1) == y2)))

oneCountSides :: [Coords] -> Coords -> Int
oneCountSides [] _ = 0
oneCountSides (cube:cubes) c
    | neighbor cube c = 1 + oneCountSides cubes c
    | otherwise = oneCountSides cubes c

countSides :: [Coords] -> [Coords] -> Int
countSides [] _ = 0
countSides (cube:cubes) checked = (6 - 2*(oneCountSides checked cube)) + countSides cubes (cube : checked)

neighborSide  :: Coords -> Coords -> (Coords,Coords)
neighborSide c1@(x1,y1,z1) c2@(x2,y2,z2) 
    | (x1 == x2) && (y1 == y2) = ((if z1 < z2 then c1 else c2),(0,0,1))
    | (z1 == z2) && (y1 == y2) = ((if x1 < x2 then c1 else c2),(1,0,0))
    | (z1 == z2) && (x1 == x2) = ((if y1 < y2 then c1 else c2),(0,1,0))

getOneNeighborSides :: Coords -> [Coords] -> [(Coords,Coords)]
getOneNeighborSides _ [] = []
getOneNeighborSides cube (c:cs)
    | neighbor cube c = neighborSide cube c : (getOneNeighborSides cube cs)
    | otherwise = getOneNeighborSides cube cs

getNeighborSides :: [Coords] -> [(Coords,Coords)]
getNeighborSides [] = []
getNeighborSides (c:cs) = (getOneNeighborSides c cs) ++ (getNeighborSides cs)

minC :: [Coords] -> Int -> Int
minC c n = minimum [ [x,y,z] !! n | (x,y,z) <- c]

maxC :: [Coords] -> Int -> Int
maxC c n = maximum [ [x,y,z] !! n | (x,y,z) <- c]

empty :: [Coords] -> [Coords]
empty cubes = [ (x,y,z) | 
                    x <- [((minC cubes 0)-1)..((maxC cubes 0)+1)], 
                    y <- [((minC cubes 1)-1)..((maxC cubes 1)+1)], 
                    z <- [((minC cubes 2)-1)..((maxC cubes 2)+1)], 
                    not ((x,y,z) `elem` cubes)]

getEmptyNeighbors :: [Coords] -> Coords -> [Coords]
getEmptyNeighbors empty (x,y,z) = filter ((flip elem) empty) neighs
    where
        neighs = [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]

walk :: [Coords] -> [Coords] -> [Coords]
-- based on https://wiki.haskell.org/99_questions/Solutions/87
walk [] _ = []
walk _ [] = []
walk cells (top:stack)
    | [x | x <- cells, x == top] == [] = walk newEmpty stack -- top already checked
    | otherwise = top : walk newEmpty (adjacent ++ stack)
    where
        adjacent = [x | x <- cells, x `elem` (getEmptyNeighbors cells top)]
        newEmpty = [x | x <- cells, x /= top]

getBubbles :: [Coords] -> [[Coords]]
getBubbles [] = []
getBubbles cells = walk cells [head cells] : (getBubbles newCells)
    where
        bubble = walk cells [head cells]
        newCells = [x | x <- cells, not (x `elem` bubble)]
