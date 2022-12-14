import System.IO.Unsafe
import Read2Darr

contents = unsafePerformIO . readFile $ "data.txt"

fileLines = lines contents


