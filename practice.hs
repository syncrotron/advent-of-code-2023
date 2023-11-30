import System.Environment
import System.IO 

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle -- Lazy; will only read file when needed. Can sut to buffer size with: hSetBuffering handle $ BlockBuffering (Just 2048)  
    let singlewords = words contents
        list = map read singlewords
        newList = map goofAround list
    mapM_ print newList
    hClose handle -- Remember to close files when done

goofAround :: Integer -> Bool
goofAround x 
    | log (fromIntegral x * 2.0) <= 1.00 = True
    | log (fromIntegral x * 2.0) > 1.00 = False