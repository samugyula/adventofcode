import System.IO

main = do
    handler <- openFile "data.txt" ReadMode
    contents <- hGetContents handler
    let
        floor = show $ countPar contents 
        basement = show $ countSteps contents 0 0
    putStr $ floor ++ "\n" ++ basement ++ "\n"
    hClose handler

countPar ""     = 0
countPar (x:xs) = case x of
    '(' -> 1 + countPar xs
    ')' -> (-1) + countPar xs
    _  -> countPar xs

countSteps x n (-1) = n
countSteps (x:xs) n nn = case x of
    '(' -> countSteps xs (n+1) (nn+1)
    ')' -> countSteps xs (n+1) (nn-1)
    __  -> countSteps xs n nn
