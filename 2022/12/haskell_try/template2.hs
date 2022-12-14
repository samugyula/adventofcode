import System.IO.Unsafe

contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents
