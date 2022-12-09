import Data.Set (member, empty, insert)
import Data.List hiding (insert)
import Data.Char
import Data.Maybe (catMaybes)
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
        res' = reduceAll [elems] invReplList
        res'' = reduceAll res' invReplList
        res''' = reduceAll res'' invReplList
        res'''' = reduceAll res''' invReplList
        res''''' = reduceAll res'''' invReplList
        res'''''' = reduceAll res''''' invReplList
        res''''''' = reduceAll res'''''' invReplList
        res'''''''' = reduceAll res''''''' invReplList
        res''''''''' = reduceAll res'''''''' invReplList
        res = reduceAll res''''''''' invReplList
{-
        res''''''' = reduceAll res'''''' invReplList
        res'''''''' = reduceAll res''''''' invReplList
        res''''''''' = reduceAll res'''''''' invReplList
        res = reduceAll res''''''''' invReplList
-}
        minL = minimum [ length x | x <- res ]
        minstr = [y | y <- res, (length y) == minL]

--  putStr $ (result res) ++ (result [minstr])
    putStr $ result [minstr]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs

contents = unsafePerformIO . readFile $ "data.txt"
fileLines = lines contents

replLines = takeWhile (/="") fileLines
molLine = last fileLines
elems = tokenize molLine

replList = makeReplList replLines
list = makeInvertedReplList replList

type Sub = ([String],String)
type Subs = [([String],String)]

makeReplList :: [String] -> [(String,[String])]
makeReplList [] = []
makeReplList (line:xs) =  case (words line) of
    (from:_:to:[]) -> (from,tokenize to) : makeReplList xs

makeInvertedReplList :: [(String,[String])] -> Subs
makeInvertedReplList [] = []
makeInvertedReplList ((x1,x2):xs) = (x2,x1) : makeInvertedReplList xs

getMatching :: Subs -> [String] -> Subs
getMatching list elems = [(x,y) | (x,y) <- list, isPrefixOf elems x]

findMatch :: [String] -> Subs -> Int -> Maybe Sub
--findMatch _ _ 9 = Nothing
findMatch elems list n = case (getMatching list initElems) of
    [] -> Nothing
    x@((x1,x2):xs) -> case([(x1,x2) | (x1,x2) <- x, x1 == initElems]) of
                    (y:[]) -> Just y
                    _ -> Nothing --findMatch elems list (n+1)
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

{-
isConflictPoint :: NSubs -> Bool
isConflictPoint ((n1,(x1,x2)):xs) = length (tillConflict (n1 + (length x1)) xs) < (length xs)

hasConflict :: NSubs -> Bool
hasConflict [] = False
hasConflict l@((n1,(x1,x2)):xs) 
    | isConflictPoint l = True
    | otherwise = hasConflict xs

removeFirstConflict :: NSubs -> NSubs
removeFirstConflict [] = []
removeFirstConflict l@(x@(n1,(x1,x2)):xs)
    | isConflictPoint l = xs
    | otherwise = x : removeFirstConflict xs

makeSubLists :: NSubs -> [NSubs]
makeSubLists subs = res
    where
        one = makeOneSubList subs
        res
            | hasConflict subs = one : ( makeSubLists $ removeFirstConflict subs )
            | otherwise = [one]
-}

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
        (frest1,srest1) = doSubs (drop nsub elems) (n+nsub) (m + 1) ys
        (frest2,srest2) = doSubs xs (n+1) m l

cucc = allPossibleSubs list elems
kek = makeSubLists cucc cucc
res1 = ordNub $ map (doSubs elems 0 0) kek

cucc2 = map (allPossibleSubs list) (map snd res1)
kek2 = map (\x -> makeSubLists x x) cucc2
res2 = map (\(y,z) -> ordNub $ map (doSubs y 0 0) z) $ zip (map snd res1) kek2

--newstrs = ordNub [hm |  hmm <- res2, (n,hm) <- hmm]

reduceAll :: [[String]] -> Subs -> [[String]]
reduceAll strs list = newstrs
    where
        cucc = map (allPossibleSubs list) strs
        kek = map (\x -> makeSubLists x x) cucc
        res = map (\(y,z) -> ordNub $ map (doSubs y 0 0) z) $ zip strs kek
        newstrs = ordNub [hm |  hmm <- res, (n,hm) <- hmm]
{-
cucc3 = map (allPossibleSubs list) (map snd res2)
kek3 = map (\x -> makeSubLists x x) cucc3
res3 = map (\(y,z) -> ordNub $ map (doSubs y 0 0) z) $ zip (map snd res2) kek3
-}

{-
prob :: NSubs -> [NSubs]
prob [x] = [[x]]
prob (x@(n1,(x1,_)):y@(n2,_):xs)
    | n1 + (length x1) <= n2 = [x : l1]
    | otherwise = [(x : h2) : (y : l2)]
    where
        l1 = last $ prob (y:xs)
        l2 = last $ prob xs
        h2 = head $ prob xs
data Tree a = Leaf a | Node1 (Tree a) | Node2 (Tree a) (Tree a) deriving (Show)

prob :: NSubs -> NSubs -> NSubs -> Tree NSubs
prob [] subs allsubs = Leaf $ makeOneSubList subs
prob [x] subs allsubs = Leaf $ makeOneSubList subs
prob (x@(n1,(x1,_)):y@(n2,_):xs) subs allsubs
    | n1 + (length x1) <= n2 = Node1 (prob (y:xs) subs allsubs)
    | otherwise = Node2 (prob xs (removeSub allsubs x) allsubs) (prob xs (removeSub allsubs y) allsubs)

getLists :: Tree NSubs -> [NSubs]
getLists (Leaf x) = [x]
getLists (Node1 l) = [] ++ (getLists l)
getLists (Node2 l r) = [] ++ (getLists l) ++ (getLists r)
-}
        
