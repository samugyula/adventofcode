{-# LANGUAGE InstanceSigs #-}                                                                                                    
import System.IO.Unsafe
import Data.List
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents

        points = readSB fileLines

        line = 2000000
        r = rangesInLine points line
        rr = sort r

        (x1res1,x2res1) = head $ getRanges rr
        res1 = x2res1 - x1res1 + 1 - (beaconInLine points line)

        (x,y) = scan points 4000000
        res2 = x*4000000 + y

    putStr $ result [res1,res2]

    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents

points = readSB fileLines


type Coords = (Int,Int)

readSB :: [String] -> [(Coords,Coords)]
readSB [] = []
readSB (l:ls) = case (words l) of
    (_:_:xS:yS:_:_:_:_:xB:yB:[]) -> ((read (drop 2 (init xS)) :: Int, 
                                      read (drop 2 (init yS)) :: Int),
                                     (read (drop 2 (init xB)) :: Int,
                                      read (drop 2 yB) :: Int)) : readSB ls
distance :: Coords -> Coords -> Int
distance (x1,y1) (x2,y2) = (abs (x1-x2)) + (abs (y1-y2))

sortPair :: Coords -> Coords
sortPair p@(x1,x2)
    | x1 < x2 = p
    | otherwise = (x2,x1)


-- abs (x1-xS) + abs (y1 - yS) = distance (xB,yB) (xS,yS)
-- abs (x1-xS) = distance (xB,yB) (xS,yS) - abs (y1 - yS)
-- | x1 > xS ->   xS  + distance (xB,yB) (xS,yS) - abs (y1 - yS)
-- | x1 < xS ->   xS  - distance (xB,yB) (xS,yS) + abs (y1 - yS)
rangesInLine :: [(Coords,Coords)] -> Int -> [Coords]
rangesInLine [] _ = []
rangesInLine ((s@(xS,yS),b@(xB,yB)):sbs) n 
    | a <= d = sortPair (xS + d - a, xS - d + a) : rangesInLine sbs n
    | otherwise = rangesInLine sbs n
    where
        d = distance (xB,yB) (xS,yS) 
        a = abs (n - yS)


-- union of ranges
unionRanges :: Coords -> Coords -> [Coords]
unionRanges r1@(x1r1,x2r1) r2@(x1r2,x2r2)

 -- (x1r1 .. x2r1)
 --                  (x1r2 .. x2r2)
    | x2r1 < x1r2 = [r1,r2]

 --                  (x1r1 .. x2r1)
 -- (x1r2 .. x2r2)
    | x2r2 < x1r1 = [r2,r1]

 -- overlapping
    | otherwise = [unionOverlapRanges r1 r2]

-- union of overlapping ranges
unionOverlapRanges :: Coords -> Coords -> Coords
unionOverlapRanges r1@(x1r1,x2r1) r2@(x1r2,x2r2)

 -- (x1r1 .................... x2r1)
 --          (x1r2 .. x2r2)         
    | x1r1 <= x1r2 && x2r1 >= x2r2 = r1

 -- (x1r2 .................... x2r2)
 --          (x1r1 .. x2r1)         

    | x1r1 >= x1r2 && x2r1 <= x2r2 = r2

 -- (x1r1 ......... x2r1)
 --         (x1r2 ......... x2r2)
    | x1r1 <= x1r2 && x2r1 >= x1r2 && x2r1 <= x2r2 = (x1r1,x2r2)

 --         (x1r1 ......... x2r1)
 -- (x1r2 ......... x2r2)
    | x1r2 <= x1r1 && x2r2 >= x1r1 && x2r2 <= x2r1 = (x1r2,x2r1)

takeWhileOverlap :: [Coords] -> [Coords]
takeWhileOverlap [x] = [x]
takeWhileOverlap (x:y:xs)
    | length (unionRanges x y) == 1 = x : takeWhileOverlap (y:xs)
    | otherwise = [x]

collapse :: [Coords] -> Coords
collapse [x] = x
collapse (x:xs) = unionOverlapRanges x (collapse xs)

getRanges :: [Coords] -> [Coords]
getRanges [] = []
getRanges xs = collapse over : getRanges (drop l xs)
    where
        over = takeWhileOverlap xs
        l = length over

getRangesReduced :: [Coords] -> [Coords]
getRangesReduced xs 
    | xs /= new = getRangesReduced new
    | otherwise = xs
    where
        new = getRanges xs

beaconInLine :: [(Coords,Coords)] -> Int -> Int
beaconInLine sbs n = length $ nub $ [ b | (_,b@(_,yB)) <- sbs, yB == n ]

scan :: [(Coords,Coords)] -> Int -> Coords
scan sbs n = scan' 0 
    where
        scan' :: Int -> Coords
        scan' nn
            | nn > n = error $ "Not found"
            | ((length res) == 2) && x < n && x >= (-1) = (x+1,nn)
            | otherwise = scan' (nn+1)
            where
                r = rangesInLine sbs nn
                rr = sort r
                res = getRangesReduced rr
                (_,x) = head res
