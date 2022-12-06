import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        lights = lightsMap 1 fileLines
        countOn = getRes lights
        lights2 = changeLights lights [(1,1),(1,100),(100,1),(100,100)]
        countOn2 = getRes lights2
    
    putStr $ result [countOn,countOn2]
    hClose handle
                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

getRes :: [Light] -> Int
getRes lights = count
    where
        newLights = recChangeState 0 lights lights
        count = length [ch | (_,ch) <- newLights, ch == '#' ]

type Light = ((Int,Int),Char)

changeLight :: [Light] -> (Int,Int) -> [Light]
changeLight ((coord,ch):xs) coord1
    | coord == coord1 = (coord,'#') : xs
    | otherwise = (coord,ch) : changeLight xs coord1

changeLights :: [Light] -> [(Int,Int)] -> [Light]
changeLights lights [] = lights
changeLights lights (x:xs) = changeLights (changeLight lights x) xs

lightsMap :: Int -> [String] -> [Light]  
lightsMap _ [] = []
lightsMap n (x:xs) = [((n,m),light) | (m,light) <- zip [1..100] x] ++ lightsMap (n+1) xs

getLight :: [Light] -> (Int, Int) -> Maybe Char
getLight [] _ = Nothing
getLight ((coords,ch):xs) coords1
    | coords == coords1 = Just ch
    | otherwise = getLight xs coords1

getVal :: Char -> [Light] -> (Int,Int) -> Int
getVal ch lights coord = case (getLight lights coord) of
    Nothing | ch == '.' -> 1
    Nothing | ch == '#' -> 0
    Just x  | x == ch -> 1
    _ -> 0

{-
getNeighborCount :: Char -> [Light] -> (Int,Int) -> Int
getNeighborCount ch lights (x,y) = 
    sum [ getVal ch lights (x1,y1) 
            | x1 <- [(x-1)..(x+1)], y1 <- [(y-1)..(y+1)],
              (x1,y1) /= (x,y) ]
-}
getNeighborCount :: Char -> [Light] -> (Int,Int) -> Int
getNeighborCount ch lights (x,y) = 
    sum [ getVal ch smallLights (x1,y1) 
            | x1 <- [(x-1)..(x+1)], y1 <- [(y-1)..(y+1)],
              (x1,y1) /= (x,y) ]
    where
        top    = drop ((x-2)*100 + y - 2) (take ((x-2)*100 + y + 1) lights)
        middle = drop ((x-1)*100 + y - 2) (take ((x-1)*100 + y + 1) lights)
        bottom = drop ((x  )*100 + y - 2) (take ((x  )*100 + y + 1) lights)
        smallLights = top ++ middle ++ bottom

changeState :: [Light] -> [Light] -> [Light]
changeState _ [] = []
changeState lights ((coord,l):xs) 
    | coord `elem` [(1,1),(1,100),(100,1),(100,100)] = (coord,'#') : rest
    | otherwise = case l of
        '#' | count `elem` [2,3] -> (coord,'#') : rest
        '#' -> (coord,'.') : rest
        '.' | count == 3 -> (coord,'#') : rest
        '.' -> (coord,'.') : rest
    where
        count = getNeighborCount '#' lights coord
        rest = changeState lights xs

recChangeState :: Int -> [Light] -> [Light] -> [Light]
recChangeState 100 _ lights = lights
recChangeState n l1 l2 = recChangeState (n+1) newLights newLights
    where
        newLights = changeState l1 l2
