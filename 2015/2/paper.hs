import Data.List
import System.IO

main = do
    handler <- openFile "data.txt" ReadMode
    contents <- hGetContents handler
    let
        fileLines = lines contents
        sumPaper = sum $ map paper $ map (split' 'x') fileLines
        sumRibbon = sum $ map ribbon $ map (split' 'x') fileLines
    putStr $ show sumPaper ++ "\n" ++ show sumRibbon ++ "\n"
    hClose handler

split' :: Char -> String -> [Int]
split' d ""  = []
split' d xs  = (read (takeWhile (/=d) xs) :: Int) 
                : (split' d ( dropWhile (==d) ( dropWhile (/=d) xs )))

paper :: [Int] -> Int
paper xs@(x:y:z:[]) = 2 * (x*y + y*z + z*x)
                        + case (sort xs) of
                            (x:y:z) -> x*y
ribbon :: [Int] -> Int
ribbon xs@(x:y:z:[]) = x*y*z + case (sort xs) of
                            (x:y:z) -> 2*(x+y)
