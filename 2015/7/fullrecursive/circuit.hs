import System.IO
import Data.Bits
import Data.Word
import Text.Read

main = do
    handler <- openFile "data.txt" ReadMode
    contents <- hGetContents handler
    let
        fileLines = lines contents
        dict = map parseLine fileLines
        val = evaluate dict ["a"]
    putStr $ show val ++ "\n"
    hClose handler

getVal :: String -> [([String],String)] -> [String]
getVal x [] = error $ "Not found: " ++ x
getVal x ((val,key):xs)
    | x == key = val
    | otherwise = getVal x xs

parseLine :: String -> ([String], String)
parseLine line = case (reverse (words line)) of
    (x:y:xs) -> (reverse xs,x)

evaluate :: [([String],String)] -> [String] -> Word16
evaluate dict (x:[]) = case (readMaybe x :: Maybe Word16) of
    Just val -> val
    Nothing  -> evaluate dict $ getVal x dict
evaluate dict (x:y:[]) = case x of
    "NOT" -> complement $ evaluate dict [y]
    _     -> error $ "Undefined behavior for " ++ y
evaluate dict (x:y:z:[]) = case y of
    "AND"    -> (evaluate dict [x]) .&. (evaluate dict [z])
    "OR"     -> (evaluate dict [x]) .|. (evaluate dict [z])
    "RSHIFT" -> shiftR (evaluate dict [x]) (fromIntegral (evaluate dict [z]) :: Int)
    "LSHIFT" -> shiftL (evaluate dict [x]) (fromIntegral (evaluate dict [z]) :: Int)
    _        -> error $ "Undefined behavior for " ++ y
