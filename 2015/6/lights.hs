import System.IO

main = do
    handler <- openFile "data.txt" ReadMode
    contents <- hGetContents handler
    let
        fileLines = lines contents
        grid_start = makeGrid 1000
        grid_end = evaluate False grid_start fileLines
        nLights = countLights grid_end
        grid_end_bright = evaluate True grid_start fileLines
        nBright = countLights grid_end_bright
    putStr $ show nLights ++ "\n" ++ show nBright ++ "\n"
    hClose handler


makeGrid :: Int -> [[Int]]
makeGrid n = replicate n $ replicate n (0 :: Int)

turnOn :: Bool -> Int -> Int
turnOn False _ = 1
turnOn True  x = x + 1

turnOff :: Bool -> Int -> Int
turnOff False _ = 0
turnOff True  x = maximum [x - 1, 0]

toogle :: Bool -> Int -> Int
toogle False x = mod (x+1) 2
toogle True  x = x + 2

execute :: (Int -> Int) -> [[Int]] -> (Int,Int) -> (Int,Int) -> [[Int]]
execute action grid (x1,y1) (x2,y2) = [ f1 xn xval | (xn,xval) <- zip [0..] grid ]
    where
        f1 xn xval
            | (xn < x1) || (xn > x2)  = xval
            | otherwise               = 
                [ f2 yn yval | (yn,yval) <- zip [0..] xval ]
                    where
                        f2 yn yval
                            | (yn < y1) || (yn > y2) = yval
                            | otherwise              = action yval

countLights :: [[Int]] -> Int
countLights grid = sum [ sum [ x | x <- yval] | yval <- grid ]

getCoords :: String -> (Int,Int)
getCoords str = ((read (takeWhile (/=',') str) :: Int),
                 (read (tail (dropWhile (/=',') str)) :: Int))

getAction :: Bool -> String -> ((Int -> Int),(Int,Int),(Int,Int))
getAction bright str = case (words str) of
    (a:b:c:d:e:[]) 
        | b == "on"  -> (turnOn bright,  getCoords c, getCoords e)
    (a:b:c:d:e:[]) 
        | b == "off" -> (turnOff bright, getCoords c, getCoords e)
    (a:b:c:d:[]) -> (toogle bright, getCoords b, getCoords d)

evaluate :: Bool -> [[Int]] -> [String] -> [[Int]]
evaluate bright grid [] = grid
evaluate bright grid (xs:str) = case (getAction bright xs) of
    (action,p1,p2) -> evaluate bright (execute action grid p1 p2) str

