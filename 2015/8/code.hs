import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        resArr = map (\x -> (length x) - (memLength x) ) fileLines
        res = sum resArr
        resArr2 = map (\x -> (allLength x) - (length x) ) fileLines
        res2 = sum resArr2
    putStr $ show res ++ "\n" ++ show res2 ++ "\n"


memLength :: String -> Int
memLength "" = -2
memLength ('\\':'x':_:_:xs) = 1 + memLength xs
memLength ('\\':_:xs) = 1 + memLength xs
memLength (_:xs) = 1 + memLength xs

allLength :: String -> Int
allLength "" = 2
allLength ('\\':xs) = 2 + allLength xs
allLength ('"':xs) = 2 + allLength xs
allLength (x:xs) = 1 + allLength xs
