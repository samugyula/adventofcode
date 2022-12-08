import Data.List
import Data.Char
import System.IO
import System.IO.Unsafe

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents

        replLines = takeWhile (/="") fileLines
        molLine = last fileLines

        replList = makeReplList replLines

        res = map fst $ recMakeSubs molLine replList ["e"] 0

    putStr $ result res
    hClose handle

contents = unsafePerformIO . readFile $ "data.txt"
fileLines = lines contents

replLines = takeWhile (/="") fileLines
molLine = last fileLines

replList = makeReplList replLines
res = map fst $ recMakeSubs molLine replList ["e"] 0
--res = recMakeSubs molLine replList ["e"] 0

countElem str elems = length [n | n <- elems, n == str]
                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


makeReplList :: [String] -> [(String,[String])]
makeReplList [] = []
makeReplList (line:xs) =  case (words line) of
    (from:_:to:[]) -> (from,tokenize to) : makeReplList xs

tokenize :: String -> [String]
tokenize "" = []
tokenize str = (head str : takeWhile nUp t) : (tokenize $ dropWhile nUp $ t)
    where
        nUp = not . isUpper
        t = tail str

makeHeadTail :: [String] -> [([String],[String])]
makeHeadTail syms = [(take n syms, drop n syms) | (n,sym) <- zip [0..] syms]

makeSub :: ([String],[String]) -> (String,[String]) -> [String]
makeSub (h,t) (sym,sub) = case (head t) of
    x | x == sym -> h ++ sub ++ (tail t)
    _ -> [""]

hasDouble :: String -> [String] -> Bool
hasDouble _ (x:[]) = False
hasDouble elem (x:y:xs)
    | x == elem && y == elem = True
    | otherwise = hasDouble elem (y:xs)

countDouble :: String -> [String] -> Int
countDouble elem elems = ct elems 0
    where
        ct :: [String] -> Int -> Int
        ct (x:[]) n = n
        ct (x:y:xs) n 
            | x == elem && y == elem = ct (y:xs) (n+1)
            | otherwise = ct (y:xs) n

checking :: [String] -> Bool
checking elems
    | elems == [""] = False
    | countDouble "Ar" elems > 1 = False
    | hasDouble "Si" elems = False
    | hasDouble "Th" elems = False
    | (countElem "Y" elems > 8) = False
    | (countElem "Ar" elems > 31) = False
    | (countElem "Rn" elems > 31) = False
    | otherwise = True

allSubs :: [String] -> [(String,[String])] -> [String]
allSubs syms replList = nub [ concat (makeSub ht sub) | ht <- makeHeadTail syms, sub <- replList, checking (makeSub ht sub) ]

allSubsInList :: [String] -> [(String,[String])] -> [String]   
allSubsInList [] _ = []
allSubsInList (x:xs) replList = allSubs (tokenize x) replList ++ (allSubsInList xs replList)

recMakeSubs :: String -> [(String,[String])] -> [String] -> Int -> [(Int,[String])]
recMakeSubs _ _ [] _ = error $ "Not found"
recMakeSubs mol replList xs n
    | any (==mol) xs = [(n, xs)]
--  | otherwise = recMakeSubs mol replList (filter f (nub (allSubsInList xs replList))) (n+1)
    | otherwise = (n,xs) : ( recMakeSubs mol replList list (n+1))
    where
        f = \x -> ((length x) <= (length mol)) 
        scr = (filter f (nub (allSubsInList xs replList)))
        maxLen = maximum [length x | x <- scr]
        list = filter (\x -> (length x) == maxLen) scr
    
