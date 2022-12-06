import System.IO

main = do
    handle <- openFile "data.txt" ReadMode
    contents <- hGetContents handle
    let
        fileLines = lines contents
        db = makeDatabase fileLines
        names = getNames db
        distances = map (calcDistance db 2503) names
        maxD = maximum distances
        initLeaderBoard = distancesAtTime db names 0
        finalRes = leaderBoardFinal db initLeaderBoard 0 2503
        maxP = maximum [ point | (name,point) <- finalRes ]
    putStr $ result [maxD,maxP]
    hClose handle

                                                                                                                                 
result :: Show a => [a] -> String
result xs = unlines $ map show xs


type Database = [(String,Int,Int,Int)]
type Leaderboard = [(String,Int)]

getNames :: Database -> [String]
getNames [] = []
getNames ((name,_,_,_):xs) = name : getNames xs

makeDatabase :: [String] -> Database
makeDatabase [] = []
makeDatabase (line:xs) = case (words line) of
    (name:_:_:speed:_:_:speedTime:_:_:_:_:_:_:restTime:_:[]) 
        -> (name, read speed :: Int, read speedTime :: Int, read restTime :: Int) : makeDatabase xs

getVals :: Database -> String -> (Int,Int,Int)
getVals ((name,speed,speedTime,restTime):xs) n
    | name == n = (speed,speedTime,restTime)
    | otherwise = getVals xs n

calcDistance :: Database -> Int -> String -> Int
calcDistance db time name = case (getVals db name) of
    (s,sT,rT) | time > (sT + rT) -> s * sT + calcDistance db (time - sT - rT) name
    (s,sT,rT) | time < sT -> s * time 
    (s,sT,rT) | time <= (sT + rT) -> s * sT

distancesAtTime :: Database -> [String] -> Int -> Leaderboard
distancesAtTime _ [] _ = []
distancesAtTime db (name:xs) time = (name, calcDistance db time name) : distancesAtTime db xs time

getDistance :: Leaderboard -> String -> Int
getDistance ((n,d):xs) name
    | n == name = d
    | otherwise = getDistance xs name


leaderBoardAtTime :: Database -> Leaderboard -> Int -> Leaderboard
leaderBoardAtTime _ [] _ = []
leaderBoardAtTime db ((name,point):xs) time 
    | getDistance distances name == maxD = (name,point+1): leaderBoardAtTime db xs time
    | otherwise                          = (name,point  ): leaderBoardAtTime db xs time
    where
        distances = distancesAtTime db (getNames db) time
        maxD = maximum [ dist | (name,dist) <- distances ]

leaderBoardFinal :: Database -> Leaderboard -> Int -> Int -> Leaderboard
leaderBoardFinal db board n end 
    | n == end = board
    | otherwise = leaderBoardFinal db (leaderBoardAtTime db board (n+1)) (n+1) end

