import System.IO
import Data.List

main = do
    let 
        mul1 = 10
        pres = [ mul1 * (sum (list f1 n)) | n <- [1..]]
        res = 1 + ( length $ takeWhile (<33100000) pres )

        mul2 = 11
        pres2 = [ mul2 * (sum (list f2 n)) | n <- [1..]]
        res2 = 1 + ( length $ takeWhile (<33100000) pres2 )

    putStr $ result [res,res2]
                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

list :: (Int -> Int -> [Int]) -> Int -> [Int]
list f n = (f 1 n) ++ (concat [f n m | m <- [2..(floor ((fromIntegral n)**0.5))], mod n m == 0])

f1 n m = case (m,factor) of
    (x,y) | x == y -> [x]
    (x,y) -> [x,y]
    where
        factor = div n m

f2 :: Int -> Int -> [Int]
f2 n m 
    | m <= 50 && factor <= 50 = case (m,factor) of
        (x,y) | x == y -> [x]
        (x,y) -> [x,y]
    | m <= 50 = [factor]
    | factor <= 50 = [m]
    | otherwise = []
    where
        factor = div n m
