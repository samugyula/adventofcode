import Data.List
import System.IO.Unsafe
import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        bottles = map (\x -> read x :: Int) fileLines
        combos = [subset | subset <- subsequences bottles, sum subset == 150 ]
        res = length combos
        combosLengths = [length subset | subset <- combos]
        minLength = minimum combosLengths
        minCombos = [length | length <- combosLengths, length == minLength]
        res2 = length minCombos

    putStr $ result [res,res2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs




