import System.IO
import Data.List

main = do
    handler <- openFile "data.txt" ReadMode
    contents <- hGetContents handler
    let
        nHouses = show $ length $ nub $ countPres 0 0 contents
        paths = twoPaths contents
        nHouses2 = show $ length $ nub $ concat $ map (countPres 0 0) paths
    putStr $ nHouses ++ "\n" ++ nHouses2 ++ "\n"
    hClose handler

countPres :: Int -> Int -> String -> [(Int,Int)]
countPres n1 n2 ""       = [(n1,n2)]
countPres n1 n2 (x:xs) = case x of
    '^' -> (n1,n2) : (countPres (n1+1) n2 xs)
    '>' -> (n1,n2) : (countPres n1 (n2+1) xs)
    'v' -> (n1,n2) : (countPres (n1-1) n2 xs)
    '<' -> (n1,n2) : (countPres n1 (n2-1) xs)
    _  -> countPres n1 n2 xs

twoPaths :: String -> [String]
twoPaths xs = (list 0) : (list 1) : []
    where
        list nn = [x | (n,x) <- zip [0..] xs, mod n 2 == nn]
