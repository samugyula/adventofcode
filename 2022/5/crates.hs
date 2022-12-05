import System.IO
import Text.Read

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        storeLines = takeWhile (not . checkIntInit) fileLines
        stackNums = getStackNums fileLines
        store = map (getStack storeLines) stackNums
        moveLines = tail $ dropWhile (/="") fileLines
        moveList = getMoves moveLines
        -- one crate per move
        finalStore = executeMoves False store moveList
        topCrates = map head finalStore
        -- all crates in one move
        finalStore2 = executeMoves True store moveList
        topCrates2 = map head finalStore2

    putStr $ topCrates ++ "\n" ++ topCrates2 ++ "\n"    
    hClose handle

checkIntInit :: String -> Bool
checkIntInit str = case (readMaybe (head (words str)) :: Maybe Int) of
    Just x  -> True
    Nothing -> False

readIntLine :: String -> [Int]
readIntLine = map (\x -> (read x :: Int)) . words

getStackNums :: [String] -> [Int]
getStackNums = readIntLine . head . dropWhile (not . checkIntInit)

getCrate :: Int -> String -> Char
getCrate n = last . take (2+(n-1)*4)

getStack :: [String] -> Int -> String
getStack [] _ = []
getStack (x:xs) n = case (getCrate n x) of
    ' ' -> getStack xs n
    val -> val : getStack xs n

pop :: String -> (Char,String)
pop (x:xs) = (x,xs)

push :: Char -> String -> String
push x xs = x:xs

takeOff :: [String] -> Int -> (Char,[String])
takeOff store n = (crate,newstore)
    where
        (crate,newstack) = pop $ store !! (n-1)
        newstore = take (n-1) store ++ [newstack] ++ drop n store

putOn :: (Char,[String]) -> Int -> [String]
putOn (crate,store) n = newstore
    where
        newstack = push crate $ store !! (n-1)
        newstore = take (n-1) store ++ [newstack] ++ drop n store

takeOffSeveral :: [String] -> Int -> Int -> (String,[String])
takeOffSeveral store n num = (crates,newstore)
    where
        stack = store !! (n-1)
        crates = take num stack
        newstack = drop num stack
        newstore = take (n-1) store ++ [newstack] ++ drop n store

putOnSeveral :: (String,[String]) -> Int -> [String]
putOnSeveral (crates,store) n = newstore
    where
        newstack = crates ++ ( store !! (n-1) )
        newstore = take (n-1) store ++ [newstack] ++ drop n store

getMoves :: [String] -> [(Int,Int,Int)]
getMoves [] = []
getMoves (x:xs) = case (words x) of
    ("move":num:"from":from:"to":to:[]) ->
        case (readIntLine (num ++ " " ++ from ++ " " ++ to)) of
            (n1:n2:n3:[]) -> (n1,n2,n3) : getMoves xs

makeOneMove :: [String] -> (Int,Int) -> [String]
makeOneMove store (from,to) = putOn (takeOff store from) to

makeMoves :: Bool -> [String] -> (Int,Int,Int) -> [String]
makeMoves False store (0,from,to) = store
makeMoves False store (n,from,to) = makeMoves False (makeOneMove store (from,to)) (n-1,from,to)
makeMoves True  store (n,from,to) = putOnSeveral (takeOffSeveral store from n) to

executeMoves :: Bool -> [String] -> [(Int,Int,Int)] -> [String]
executeMoves _ store [] = store
executeMoves several store (x:xs) = executeMoves several (makeMoves several store x) xs
