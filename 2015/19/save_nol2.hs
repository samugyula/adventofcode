import Data.Set (member, empty, insert)
import Data.List hiding (insert)
import Data.Char
import System.IO
import System.IO.Unsafe
-- Taken From Yi
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go empty l
  where
    go _ []     = []
    go s (x:xs) = if x `member` s then go s xs
                                      else x : go (insert x s) xs


main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents

        replLines = takeWhile (/="") fileLines
        molLine = last fileLines
        elems = tokenize molLine

        replList = makeReplList replLines
        invReplList = makeInvertedReplList replList

        res = recBacktrackAll [([""],elems)] invReplList 0

    putStr $ result res
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

contents = unsafePerformIO . readFile $ "data.txt"
fileLines = lines contents

replLines = takeWhile (/="") fileLines
molLine = last fileLines
elems = tokenize molLine

replList = makeReplList replLines
invReplList = makeInvertedReplList replList

res = recBacktrackAll [([""],elems)] invReplList 0

countElem str elems = length [n | n <- elems, n == str]

type Sub = ([String],String)
type Subs = [([String],String)]

makeInvertedReplList :: [(String,[String])] -> Subs
makeInvertedReplList [] = []
makeInvertedReplList ((x1,x2):xs) = (x2,x1) : makeInvertedReplList xs

getMatching :: Subs -> [String] -> Subs
getMatching list elems = [(x,y) | (x,y) <- list, isPrefixOf elems x]

findMatch :: [String] -> Subs -> Int -> Maybe Sub
--findMatch _ _ 9 = Nothing
findMatch elems list n = case (getMatching list initElems) of
    [] -> Nothing
    ((x@(x1,x2)):[]) | elems == x1 -> Just x
    x@((x1,x2):xs) -> case([(x1,x2) | (x1,x2) <- x, x1 == initElems]) of
                    (y:[]) -> Just y
                    _ -> Nothing --findMatch elems list (n+1)
    where
        initElems = take n elems

type Tup = ([String],[String])

findAllMatch :: [String] -> Subs -> Int -> [Maybe Sub]
findAllMatch _ _ 9 = []
findAllMatch elems list n = case (findMatch elems list n) of 
    Just x -> (Just x) : rest
    Nothing -> rest
    where
        rest = findAllMatch elems list (n +1)

backtrack :: Tup -> Subs -> [Tup]
backtrack (_,[]) _ = []
backtrack (h,t) list = case (findAllMatch t list 1) of
    [] -> rest
    x -> subThem (h,t) x ++ rest
    where
        rest = backtrack (h ++ [head t], tail t) list
        subThem :: Tup -> [Maybe Sub] -> [Tup]
        subThem _ [] = [] 
        subThem (h,t) (one:xs) = case one of
            Nothing -> subThem (h,t) xs
            Just (y,"e") | h == [""] && y == t -> ([""],((tail h) ++ ("e" : (drop (length y) t)))) : subThem (h,t) xs
            Just (y,"e") -> subThem (h,t) xs
            Just (y,x) | x /= "e" -> ([""],((tail h) ++ (x : (drop (length y) t)))) : subThem (h,t) xs

backtrackAll :: [Tup] -> Subs -> [Tup]
backtrackAll [] _ = []
backtrackAll (x:xs) list = backtrack x list ++ (backtrackAll xs list)
{-
    where
        res1 :: Tup -> [Tup] -> [Tup]
        res1 y ys = backtrack y list ++ (backtrackAll ys list)
        res :: [Tup] -> [Tup]
        res ys = ordNub ys
        final = res $ res1 x xs
-}

recBacktrackAll :: [([String],[String])] -> Subs -> Int -> [(Int,Int,Int)]
recBacktrackAll xs list n 
    | xs == [] = error $ "Not found"
    | n == 33 = [cucc]
    | all (==([""],["e"])) xs = [cucc]
    | otherwise = cucc : recBacktrackAll res list (n+1)
    where 
        cucc = (n,length xs,minimum [length y | (x,y) <- xs])
--      res = backtrackAll xs list
        res1 = ordNub $ backtrackAll xs list
        minL = minimum [length y | (x,y) <- res1]
        res = filter (\x -> (length (snd x)) == minL) res1

makeReplList :: [String] -> [(String,[String])]
makeReplList [] = []
makeReplList (line:xs) =  case (words line) of
    (from:_:to:[]) -> (from,tokenize to) : makeReplList xs

tokenize :: String -> [String]
tokenize "" = []
tokenize str = (head str : takeWhile nUp t) : (tokenize $ dropWhile nUp $ t)
    where
        nUp = not . isUpper
        t = tail str

makeSub :: Tup -> (String,[String]) -> [String]
makeSub (h,t) (sym,sub) = case (head t) of
    x | x == sym -> h ++ sub ++ (tail t)
    _ -> [""]


