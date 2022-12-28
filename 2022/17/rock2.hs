import System.IO
import System.IO.Unsafe

main = do
    handle <- openFile "data.txt" ReadMode
    hSetBuffering stdout NoBuffering
    contents <- hGetContents handle

    let
        jets = init contents

        grid = replicate 7 (replicate 7 '.') ++ [replicate 7 '#']

        tower = doFalls (cycle jets) grid 2022
        height = (length tower) - 1 - (highest tower)

--      gets periodic after 981 (tried manually with interval halving)
        nRep = div (10^12 - 981) 1725
        nRem = mod (10^12 - 981) 1725
        tow = doFalls (cycle jets) grid 981
        tow2 = doFalls (cycle jets) grid (981 + 1725)
        tow3 = doFalls (cycle jets) grid (981 + nRem)
        l1 = (length tow) - 1 - (highest tow)
        l2 = (length tow2) - 1 - (highest tow2) - l1
        l3 = (length tow3) - 1 - (highest tow3) - l1

    putStr $ result [height,l1 + nRep*l2 + l3]

    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

sh :: [String] -> IO ()
sh x = putStr $ unlines x

highest :: [String] -> Int
highest str = h str 0
    where
        h [] n = n
        h (x:xs) n
            | '#' `elem` x = n
            | otherwise = h xs (n+1)

type Coords = (Int,Int)

rockInit :: Int -> [Coords]
rockInit n = case n of
    0 -> [(2,3),(3,3),(4,3),(5,3)]
    1 -> [(3,3),(2,2),(3,2),(4,2),(3,1)]
    2 -> [(2,3),(3,3),(4,3),(4,2),(4,1)]
    3 -> [(2,0),(2,1),(2,2),(2,3)]
    4 -> [(2,3),(3,3),(2,2),(3,2)]


rock :: [Coords] -> [String]
rock c = [ [ if (x,y) `elem` c then '#' else '.' | x <- [0..6] ] | y <- [0..3] ]

coll :: [String] -> [String] -> Bool
coll _ [] = False
coll (g:grid) (r:rock) 
    | any (==('#','#')) (zip r g) = True
    | otherwise = coll grid rock

getCoords :: [String] -> [Coords]
getCoords rock = concat  [ 
                 [ (x,y) | (x,c) <- zip [0..] line, c == '#'] 
                 | (y,line) <- zip [0..] rock 
                ]

shift :: [String] -> Char -> [String] -> [String]
shift r c grid = case c of
    '<' | left -> r
    '>' | right -> r
    _ | coll grid (rock newcoords) -> r
    _ -> rock newcoords
    where
        coords = getCoords r
        right = any (\(x,_) -> x == 6) coords
        left = any (\(x,_) -> x == 0) coords
        newcoords = case c of
            '<' -> map (\(x,y) -> (x-1,y)) coords
            '>' -> map (\(x,y) -> (x+1,y)) coords

fall2 :: Int -> String -> [String] -> [String] -> [String] -> (Int,String,[String])
fall2 n jets orig grid r = if (coll (tail grid) shifted)
            then ((n+1),(tail jets), extra ++
                       skipped ++ update)
            else fall2 (n+1) (tail jets) orig (tail grid) shifted
    where
        shifted = shift r (head jets) (take 4 grid)
        update' = (merge shifted (take 4 grid)) ++ (drop 4 grid)
        update = drop (highest update') update'
        skipped = take ((length orig) - (length update)) orig
        extra = case (7 - (highest skipped)) of
            x | x > 0 -> replicate x (replicate 7 '.')
            _ -> []

fall3 :: String -> [String] -> [String] -> [String] -> Maybe (String,[String])
fall3 jets orig grid r 
    | jets == "" = Nothing
    | otherwise = if (coll (tail grid) shifted)
            then Just ((tail jets), extra ++
                       skipped ++ update)
            else fall3 (tail jets) orig (tail grid) shifted
    where
        shifted = shift r (head jets) (take 4 grid)
        update' = (merge shifted (take 4 grid)) ++ (drop 4 grid)
        update = drop (highest update') update'
        skipped = take ((length orig) - (length update)) orig
        extra = case (7 - (highest skipped)) of
            x | x > 0 -> replicate x (replicate 7 '.')
            _ -> []

fall :: String -> [String] -> [String] -> [String] -> (String,[String])
fall jets orig grid r = if (coll (tail grid) shifted)
            then ((tail jets), extra ++
                       skipped ++ update)
            else fall (tail jets) orig (tail grid) shifted
    where
        shifted = shift r (head jets) (take 4 grid)
        update' = (merge shifted (take 4 grid)) ++ (drop 4 grid)
        update = drop (highest update') update'
        skipped = take ((length orig) - (length update)) orig
        extra = case (7 - (highest skipped)) of
            x | x > 0 -> replicate x (replicate 7 '.')
            _ -> []

        

merge :: [String] -> [String] -> [String]
merge r g = [ [ if (c1 == '#' || c2 == '#') 
                then '#' else '.' | (c1,c2) <- zip l1 l2 ]
              | (l1,l2) <- zip r g ]

doFalls :: String -> [String] -> Int -> [String]
doFalls jets grid nn = g
    where
        doFalls' :: String -> [String] -> Int -> (String,[String])
        doFalls' jets' grid' n
            | n == (nn-1) = (j'', g'')
            | otherwise = doFalls' j'' g'' (n+1)
            where
                (j'', g'') = fall jets' grid' grid' (rock $ rockInit (mod n 5))
        (j,g) = doFalls' jets grid 0


doFalls2 :: String -> [String] -> Int -> Int
doFalls2 jets grid nn = db
    where
        doFalls2' :: Int -> String -> [String] -> Int -> (Int,String,[String])
        doFalls2' nnn jets' grid' n
            | (nn-1) == n  = (nnn', j'', g'')
            | otherwise = doFalls2' nnn' j'' g'' (n+1)
            where
                (nnn', j'', g'') = fall2 nnn jets' grid' grid' (rock $ rockInit (mod n 5))
        (db,j,g) = doFalls2' 0 jets grid 0

doFalls3 :: String -> [String] -> Maybe (Int,[String])
doFalls3 jets grid = doFalls3' jets grid 0
    where
        doFalls3' :: String -> [String] -> Int -> Maybe (Int,[String])
        doFalls3' jets' grid' n = case x of
            Nothing -> Nothing
            Just (j'', g'') | length j'' == 0 -> Just (n+1, g'')
            Just (j'', g'') -> doFalls3' j'' g'' (n+1)
            where
                x = fall3 jets' grid' grid' (rock $ rockInit (mod n 5))

check :: String -> [String] -> Int -> Maybe Int
check jets grid n = l
    where
        gr = doFalls (cycle jets) grid n
        nSteps = doFalls2 (cycle jets) grid n
        jets' = take (nSteps + (length jets)) (cycle jets)
        l = case (doFalls3 jets' grid) of
            Nothing -> Nothing
            Just (nn,x) | (take 20 gr) == (take 20 x) -> Just (nn-n)
            _ -> Nothing

checkAll :: String -> [String] -> [Maybe (Int,Int)]
checkAll jets grid = checkAll' 1
    where
        checkAll' :: Int -> [Maybe (Int,Int)]
        checkAll' n = case (check jets grid n) of
            Nothing -> Nothing : checkAll' (n+1)
            Just nn -> [Just (n,nn)]
