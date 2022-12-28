
import System.IO.Unsafe

contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents

pipes = readPipes fileLines

type Pipe = (String,Int,[String])

readPipes :: [String] -> [Pipe]
readPipes [] = []
readPipes (x:xs) = (name,flow,pipes) : readPipes xs
    where
        str = words x
        name = str !! 1
        flow = read (init $ drop 5 $ str !! 4) :: Int
        pipes = map (filter (/=',')) $ drop 9 str

data Tree a = Leaf a | Node [Tree a] deriving (Show)

buildTree :: [Pipe] -> Pipe -> Int -> [String] -> Tree [String]
buildTree _ _ 0 collect = Leaf collect
buildTree pipes pipe@(name,flow,names) n collect 
    | ("open"++name) `elem` collect || flow == 0 = Node treeList
    | otherwise = Node ((getNewTree pipe n "open"):treeList)

    where
        getNewTree :: Pipe -> Int -> String -> Tree [String]
        getNewTree pipe'@(name',_,_) n action = buildTree pipes pipe' (n-1) ((action++name'):collect)
        getPipes = filter (\(name',_,_) -> name' `elem` names) pipes
        treeList = [getNewTree pipe' n "walk" | pipe' <- getPipes]

