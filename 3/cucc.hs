import Data.List

main = do  
    contents <- getContents  
    let 
        fileLines = lines contents
        halvesList = halves fileLines
        sectsList = sects halvesList
        prList = getPrior sectsList
    putStr $ (show (sum prList)) ++ "\n"
  
lhalf :: String -> Int
lhalf str = (length str) `div` 2

first :: String -> String
first str = take (lhalf str) str
  
second :: String -> String
second str = drop (lhalf str) str

halves :: [String] -> [(String,String)]
halves fl = [(first line, second line) | line <- fl] 

sects :: [(String,String)] -> [Char]
sects fl = [(nub (intersect fh sh)) !! 0 | (fh,sh) <- fl]

priorities = zip (['a'..'z'] ++ ['A'..'Z']) [1..]

getVal :: [(Char,Int)] -> Char -> Int
getVal ((key,val):xs) x
    | x == key  = val
    | otherwise = getVal xs x

getPrior = map (getVal priorities)
