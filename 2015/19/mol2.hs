import Data.Set (insert,member,empty)
import Data.List hiding (insert)
import Data.Char
import System.IO

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
        allSubs = [((fromIntegral n :: Int),t) | (n,x) <- zip [0..] elems, t@(y1,y2) <- replList, [x] == y1]
        res1 = length $ nub $ subAllForward elems allSubs

        invReplList = makeInvertedReplList replList

        res2' = recReduceAll [(0,elems)] invReplList
        res2 = minimum [n+1 | (n,x) <- res2']

    putStr $ result [res1,res2]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

type Sub = ([String],String)
type Subs = [([String],String)]

makeReplList :: [String] -> Subs
makeReplList [] = []
makeReplList (line:xs) =  case (words line) of
    (from:_:to:[]) -> ([from],to) : makeReplList xs

makeInvertedReplList :: Subs -> Subs
makeInvertedReplList [] = []
makeInvertedReplList ((x1,x2):xs) = (tokenize x2,head x1) : makeInvertedReplList xs

getMatching :: Subs -> [String] -> Subs
getMatching list elems = [(x,y) | (x,y) <- list, isPrefixOf elems x]

findMatch :: [String] -> Subs -> Int -> Maybe Sub
findMatch elems list n = case (getMatching list initElems) of
    [] -> Nothing
    x@((x1,x2):xs) -> case([(x1,x2) | (x1,x2) <- x, x1 == initElems]) of
                    (y:[]) -> Just y
                    _ -> Nothing 
    where
        initElems = take n elems

type Tup = ([String],[String])

findAllMatch :: [String] -> Subs -> Maybe Sub
findAllMatch elems list = findAllMatch' 9
    where
        findAllMatch' :: Int -> Maybe Sub
        findAllMatch' 0 = Nothing
        findAllMatch' n
            | n > (length elems) = rest
            | otherwise = case (findMatch elems list n) of 
                Just (_,"e") -> rest
                Just x -> Just x
                Nothing -> rest
            where
                rest = findAllMatch' (n -1)


tokenize :: String -> [String]
tokenize "" = []
tokenize str = (head str : takeWhile nUp t) : (tokenize $ dropWhile nUp $ t)
    where
        nUp = not . isUpper
        t = tail str


type NSub = (Int,Sub)
type NSubs = [NSub]

filterNothing :: [(Int, Maybe Sub)] -> NSubs
filterNothing [] = []
filterNothing ((n,x):xs) = case x of
    Nothing -> filterNothing xs
    Just y -> (n,y) : filterNothing xs

allPossibleSubs :: Subs -> [String] -> NSubs
allPossibleSubs list elems = filterNothing $ zip [0..] $ map ((flip findAllMatch) list) (init $ tails elems)

tillConflict :: Int -> NSubs -> NSubs
tillConflict n = dropWhile (\(n3,_) -> n > n3)

makeOneSubList :: NSubs -> NSubs
makeOneSubList [] = []
makeOneSubList (x:[]) = [x]
makeOneSubList (x@(n1,(x1,_)):y@(n2,_):xs)
    | n1 + (length x1) <= n2 = x : (makeOneSubList (y:xs))
    | otherwise = x : (makeOneSubList $ tillConflict (n1 + (length x1)) xs)

removeSub :: NSubs -> NSub -> NSubs
removeSub (x:xs) sub
    | x == sub = xs
    | otherwise = x : (removeSub xs sub)

makeSubLists :: NSubs -> NSubs -> [NSubs]
makeSubLists [] alls = []
makeSubLists [x] alls = [[x]]
makeSubLists (x@(n1,(x1,_)):y@(n2,_):xs) alls
    | n1 + (length x1) <= n2 = makeSubLists xs alls
    | otherwise = (makeOneSubList (r x)) : (makeOneSubList (r y)) : (makeSubLists xs alls)
    where 
        r = removeSub alls

doSubs :: [String] -> Int -> Int -> NSubs -> (Int,[String])
doSubs elems _ m [] = (m,elems)
doSubs elems@(x:xs) n m l@((p,(s1,s2)):ys)
    | p == n = (frest1, s2 : srest1)
    | otherwise = (frest2, x : srest2)
    where
        nsub = length s1
        (frest1,srest1) 
            | ys /= [] && (fst (head ys)) == p = doSubs elems n m ys
            | otherwise = doSubs (drop nsub elems) (n+nsub) (m + 1) ys
        (frest2,srest2) = doSubs xs (n+1) m l

reduceAll :: [(Int,[String])] -> Subs -> [(Int,[String])]
reduceAll strs list = newstrs
    where
        juststrs = map snd strs
        cucc = map (allPossibleSubs list) juststrs
        kek = map (\x -> makeSubLists x x) cucc
        res = map (\((m,y),z) -> ordNub $ map (doSubs y 0 m) z) $ zip strs kek
        newstrs = ordNub [hm |  hmm <- res, hm <- hmm]

eSubs :: Subs -> [[String]]
eSubs list = [ x | (x,"e") <- list]

recReduceAll :: [(Int,[String])] -> Subs -> [(Int,[String])]
recReduceAll strs list
    | any (\x -> x `elem` (eSubs list)) (map snd strs) = [t | t@(n,x) <- strs, x `elem` (eSubs list)]
    | otherwise = recReduceAll (reduceAll strs list) list
        
subAllForward :: [String] -> NSubs -> [[String]]
subAllForward elems [] = []
subAllForward elems ((n,(s1,s2)):xs) = ((take n elems) ++ (tokenize s2) ++ (drop (n+1) elems)) : (subAllForward elems xs)
