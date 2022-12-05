import Data.List
import System.IO

main = do
    let 
        pass1 = incrTillGood inp
        pass2 = incrTillGood $ incr pass1
    putStr $ result [pass1,pass2]

inp = "hxbxwxba"
                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


incrRev :: String -> String
incrRev "zzzzzzzz" = "aaaaaaaa"
incrRev "" = ""
incrRev (x:xs) = case x of
    'z' -> 'a' : incrRev xs
    c   -> succ c : xs

incr :: String -> String
incr = reverse . incrRev . reverse

three :: String -> Bool
three (x:y:[]) = False
three (x:y:z:xs) 
    | isInfixOf (x:y:z:[]) ['a'..'z'] = True
    | otherwise = three (y:z:xs)

forbidden :: String -> Bool
forbidden = any (flip elem "iol")

hasDouble :: String -> Bool
hasDouble ""     = False
hasDouble (_:"") = False
hasDouble (x:y:xs)
    | x == y = True
    | otherwise = hasDouble (y:xs)

hasTwoDoubles :: String -> Bool
hasTwoDoubles (x:y:z:"") = False
hasTwoDoubles (x:y:xs)
    | x == y = hasDouble xs
    | otherwise = hasTwoDoubles (y:xs)

goodPassword :: String -> Bool
goodPassword str = hasTwoDoubles str && (not . forbidden) str && three str

incrAfterForbidden :: String -> String
incrAfterForbidden str = first ++ [succ bad] ++ replicate ((length last) - 1) 'a'
    where
        check = (\x -> not (x `elem` "iol"))
        first = takeWhile check str
        last  = dropWhile check str
        bad   = head last 

incrTillGood :: String -> String
incrTillGood str 
    | forbidden str = incrTillGood $ incrAfterForbidden str
    | otherwise = case (goodPassword str) of
        True -> str
        False -> incrTillGood $ incr str
