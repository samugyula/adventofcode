import System.IO.Unsafe
import Data.List
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents

        paths = readPaths fileLines

        maxx = maximum [ maximum [ x | (x,y) <- path ] | path <- paths ]
        minx = minimum [ minimum [ x | (x,y) <- path ] | path <- paths ]
        xSize = maxx - minx + 3

        maxy = maximum [ maximum [ y | (x,y) <- path ] | path <- paths ]
        ySize = maxy + 2

        trfPaths = [ [ (x-minx+1,y) | (x,y) <- path ] | path <- paths ]

        initGrid = [ replicate xSize '.' | n <- [1..ySize] ]

        grid = updateWithPaths initGrid trfPaths

        filled = fillWithSand grid sizes
        res1 = countSand filled

        paths' = [(minx-1,maxy+2),(maxx+1,maxy+2)] : paths
        trfPaths' = [ [ (x-minx+1,y) | (x,y) <- path' ] | path' <- paths' ]
        sizes'@(xSize',ySize') = (maxx - minx + 3,maxy+3)
        initGrid' = [ replicate xSize' '.' | n <- [1..ySize'] ]
        grid' = updateWithPaths initGrid' trfPaths'
        filledUp = fillUpWithSand grid' sizes' entry
        res2 = 1 + (countSand filledUp)

    putStr $ result [res1,res2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents

paths = readPaths fileLines

maxx = maximum [ maximum [ x | (x,y) <- path ] | path <- paths ]
minx = minimum [ minimum [ x | (x,y) <- path ] | path <- paths ]
xSize = maxx - minx + 3

maxy = maximum [ maximum [ y | (x,y) <- path ] | path <- paths ]
ySize = maxy + 2

sizes = (maxx,maxy)
entry = (500 - minx + 1, 0 :: Int)

trfPaths = [ [ (x-minx+1,y) | (x,y) <- path ] | path <- paths ]

initGrid = [ replicate xSize '.' | n <- [1..ySize] ]

grid = updateWithPaths initGrid trfPaths

type Coords = (Int,Int)
type Map = [[Char]]

readPaths :: [String] -> [[Coords]]
readPaths [] = []
readPaths (x:xs) = path : readPaths xs
    where
        points = filter (/="->") $ words x
        pathStr = map (break (==',')) points
        path = map (\(x,y) -> (read x :: Int, read (tail y) :: Int)) pathStr

updateWithPaths :: Map -> [[Coords]] -> Map
updateWithPaths grid [] = grid
updateWithPaths grid (x:xs) = updateWithPaths (updateWithPath grid x) xs

updateWithPath :: Map -> [Coords] -> Map
updateWithPath grid [x] = grid
updateWithPath grid (x:y:xs) = updateWithPath (updateWithLine grid x y) (y:xs)

updateWithLine :: Map -> Coords -> Coords -> Map
updateWithLine grid p1 p2@(x2,y2) = updateWithLine' grid p1
    where
        updateWithLine' :: Map -> Coords -> Map
        updateWithLine' grid p1@(x1,y1)
            | x1 == x2 && y1 == y2 = newGrid 
            | x1 == x2 && y1 <  y2 = updateWithLine' newGrid (x1,y1+1)
            | x1 == x2 && y1 >  y2 = updateWithLine' newGrid (x1,y1-1)
            | x1 <  x2 && y1 == y2 = updateWithLine' newGrid (x1+1,y1)
            | x1 >  x2 && y1 == y2 = updateWithLine' newGrid (x1-1,y1)
            where
                newGrid = getNewGrid grid p1 '#'

getNewGrid :: Map -> Coords -> Char -> Map
getNewGrid grid (x1,y1) s = (take y1 grid) 
            ++ (((take x1 (grid !! y1)) ++ s : (drop (x1+1) (grid !! y1))) 
            : (drop (y1+1) grid))

-- extend grid dynamically for second part
getStable2 :: Map -> Coords -> Coords -> Coords -> (Map, Coords, Coords, Coords)
getStable2 grid dims@(mx,my) p@(x,y) ent@(e1,e2)
    | (grid' !! (y'+1) !! (x')) == '.' = getStable2 grid' dims' (x',y'+1) ent'
    | (grid' !! (y'+1) !! (x'-1)) == '.' = getStable2 grid' dims' (x'-1,y'+1) ent'
    | (grid' !! (y'+1) !! (x'+1)) == '.' = getStable2 grid' dims' (x'+1,y'+1) ent'
    | otherwise = (grid', p', dims', ent')
        where
            (p'@(x',y'),dims',grid',ent')
                | x == 0 = ((1,y),(mx+1,my),[(if n == my then '#' else '.'):l | (n,l) <- zip [1..] grid],(e1+1,e2))
                | x == (mx-1) = ((x,y),(mx+1,my),[l ++ (if n == my then "#" else ".") | (n,l) <- zip [1..] grid],ent)
                | otherwise = (p,dims,grid,ent)

getStable :: Map -> Coords -> Coords -> Coords
getStable grid dims@(mx,my) p = getStable' p
    where
        getStable' :: Coords -> Coords
        getStable' p@(x,y)
            | (x == 0) || (x == mx) || (y == my) = p
            | (grid !! (y+1) !! (x)) == '.' = getStable' (x,y+1)
            | (grid !! (y+1) !! (x-1)) == '.' = getStable' (x-1,y+1)
            | (grid !! (y+1) !! (x+1)) == '.' = getStable' (x+1,y+1)
            | otherwise = p

fillWithSand :: Map -> Coords -> Map
fillWithSand grid dims@(mx,my) = case (getStable grid dims entry) of
    p@(x,y) | (x == 0) || (x == mx) || (y == my) -> grid
    p -> fillWithSand (getNewGrid grid p 'o') dims 

fillUpWithSand :: Map -> Coords -> Coords -> Map
fillUpWithSand grid dims@(mx,my) entry = case (getStable2 grid dims entry entry) of
    (grid',p, dims', entr') | p == entry -> grid'
    (grid',p, dims', entr') -> fillUpWithSand (getNewGrid grid' p 'o') dims' entr'

countSand :: Map -> Int
countSand grid = sum [ length [ x | x <- row, x == 'o'] | row <- grid ]

{-
fillUpWithSand' :: Map -> Coords -> Coords -> Int -> Int -> Map
fillUpWithSand' grid dims@(mx,my) entry n m = case (getStable2 grid dims entry entry) of
    (grid',p, dims', entr') | n == m -> grid'
    (grid',p, dims', entr') -> fillUpWithSand' (getNewGrid grid' p 'o') dims' entr' (n+1) m

showIt :: Map -> IO ()
showIt grid = putStr $ unlines grid
-}
