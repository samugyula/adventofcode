import Data.List
import System.IO.Unsafe
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        nums = zip [(1 :: Int)..] $ readNums fileLines
        mixed = map snd $ mix nums (length nums)
        from0 = dropWhile (/=0) (cycle mixed)
        res = [ from0 !! n | n <- [1000,2000,3000] ]

        nums2 = [ (n,811589153*x) | (n,x) <- nums]
        mixed2 = map snd $ mixRepeat nums2 10
        from02 = dropWhile (/=0) (cycle mixed2)
        res2 = [ from02 !! n | n <- [1000,2000,3000] ]

    putStr $ result [sum res, sum res2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

readNums :: [String] -> [Int]
readNums [] = []
readNums (l:ls) = (read l :: Int) : readNums ls

type N = (Int,Int)
type Ns = [N]

mixOne :: Int -> Ns -> Ns
mixOne n ns = (take h without) ++ v : drop h without
    where
        mixed@(p,v@(_,x)) = head $ [ (p1,val) | (p1,val@(n1,_)) <- zip [1..] ns , n1 == n ]
        xx = mod (x + p) ((length ns) - 1)
        h
            | xx > ((length ns) - 1)  = xx - 1 - (length ns) + 1
            | xx < 2  = (length ns) - (1 - xx) - 1
            | otherwise = xx - 1
        without = [ x | x <- ns, x /= v ]

mix :: Ns -> Int -> Ns
mix ns n = mix' ns 1
    where
        mix' :: Ns -> Int -> Ns
        mix' ns' n'
            | n' > n = ns'
            | otherwise = mix' (mixOne n' ns') (n'+1) 

mixRepeat :: Ns -> Int -> Ns
mixRepeat ns n = mix' ns 1
    where
        mix' :: Ns -> Int -> Ns
        mix' ns' n'
            | n' > n = ns'
            | otherwise = mix' (mix ns' (length ns)) (n'+1) 

