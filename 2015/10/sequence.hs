import System.IO

main = do
    let
        res1 = doSeq inp 40
        res2 = doSeq res1 10
    putStr $ result [length res1,length res2]
                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

inp :: String
inp = "1113122113"

doSeq :: String -> Int -> String
doSeq str 0 = str
doSeq str n = doSeq (seqFromString str) (n-1)

parts :: String -> [String]
parts "" = []
parts l@(x:xs) = first : parts ( drop (length first) l)
    where
        first = takeWhile (==x) l

seqFromParts :: [String] -> String
seqFromParts [] = ""
seqFromParts (x:xs) = show (length x) ++ [head x] ++ seqFromParts xs

seqFromString :: String -> String
seqFromString = seqFromParts . parts
