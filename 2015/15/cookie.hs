import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        db = makeDB fileLines
        l = [0..100]
        combos = [[n1 :: Int,n2 :: Int,n3 :: Int,n4 :: Int] | n1 <- l, n2 <- l, n3 <- l, n4 <- l, n1 + n2 + n3 + n4 == 100]
        allres1 = map (res1 db) combos
        r1 = maximum allres1
        allres2 = map (res2 db) combos
        r2 = maximum allres2
    putStr $ result [r1,r2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

readInt' :: String -> Int
readInt' x = read x :: Int

readInt :: String -> Int
readInt x = read (init x) :: Int

type Database = [(String,[Int])]

makeDB :: [String] -> Database
makeDB [] = []
makeDB (line:xs) = case (words line) of
    (name:_:n1:_:n2:_:n3:_:n4:_:n5:[]) -> (init name, [readInt n1, readInt n2, readInt n3, readInt n4, readInt' n5]) : makeDB xs

getCol :: Database -> Int -> [Int]
getCol [] _ = []
getCol ((name,list):xs) n = (list !! n) : getCol xs n

mult' :: [Int] -> [Int] -> Int
mult' [] [] = 0
mult' (x:xs) (y:ys) = x*y + mult' xs ys

mult :: [Int] -> [Int] -> Int
mult xs ys = maximum [mult' xs ys,0]

resrec :: Int -> Database -> [Int] -> Int
resrec 0 _ _ = 1
resrec n db recipe = mult recipe (getCol db (n-1)) * resrec (n-1) db recipe

res1 :: Database -> [Int] -> Int
res1 db recipe = resrec 4 db recipe

res2 :: Database -> [Int] -> Int
res2 db recipe 
    | (mult recipe (getCol db 4)) == 500 = resrec 4 db recipe
    | otherwise = 0
