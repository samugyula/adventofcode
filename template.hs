import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle

    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

