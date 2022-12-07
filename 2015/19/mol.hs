import Data.List
import Data.Char
import System.IO

main = do
    handle <- openFile "data2.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents

        replLines = takeWhile (/="") fileLines
        molLine = last fileLines

        replList = makeReplList replLines

        res = fst $ recMakeSubs molLine replList ["e"] 0

    putStr $ result [res]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


makeReplList :: [String] -> [(String,String)]
makeReplList [] = []
makeReplList (line:xs) =  case (words line) of
    (from:_:to:[]) -> (from,to) : makeReplList xs

tokenize :: String -> [String]
tokenize "" = []
tokenize str = (head str : takeWhile nUp t) : (tokenize $ dropWhile nUp $ t)
    where
        nUp = not . isUpper
        t = tail str

makeHeadTail :: String -> [(String,String)]
makeHeadTail str = [(concat (take n syms), concat (drop n syms)) | (n,sym) <- zip [0..] syms]
    where
        syms = tokenize str

makeSub :: (String,String) -> (String,String) -> String
makeSub (h,t) (sym,sub) = case (head syms) of
    x | x == sym -> h ++ sub ++ (concat $ tail syms)
    _ -> ""
    where
        syms = tokenize t

allSubs :: String -> [(String,String)] -> [String]
allSubs str replList = nub [ makeSub ht sub | ht <- makeHeadTail str, sub <- replList, (makeSub ht sub) /= "" ]

allSubsInList :: [String] -> [(String,String)] -> [String]   
allSubsInList [] _ = []
allSubsInList (x:xs) replList = allSubs x replList ++ (allSubsInList xs replList)

recMakeSubs :: String -> [(String,String)] -> [String] -> Int -> (Int,[String])
recMakeSubs _ _ [] _ = error $ "Not found"
recMakeSubs mol replList xs n
    | any (==mol) xs = (n, xs)
    | otherwise = recMakeSubs mol replList (filter (\x -> (length x) <= (length mol)) (nub (allSubsInList xs replList))) (n+1)
    
