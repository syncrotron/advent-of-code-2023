-- Authored by Sam Horovatin

import System.Environment
import System.IO 
import Text.Read (readMaybe)

-- Start Part 1 --

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle -- Lazy; will only read file when needed. Can sut to buffer size with: hSetBuffering handle $ BlockBuffering (Just 2048)  
    let filelines = words contents
    print (sum (map (gen2DigitNum . condenseInts) filelines))
    -- mapM_ print filelines
    hClose handle -- Remember to close files when done

condenseInts :: String -> [Int]
condenseInts xs = case xs of [] -> [] 
                             [x] -> case readMaybe [x] :: Maybe Int of Nothing -> []
                                                                       Just x' -> [x']
                             x:xs -> case readMaybe [x] :: Maybe Int of Nothing -> condenseInts xs
                                                                        Just x'  -> x' : condenseInts xs

gen2DigitNum :: [Int] -> Int
gen2DigitNum xs = case xs of [] -> 0
                             [x] -> read (show x ++ show x)
                             x:xs -> read (show x ++ show (last xs))

-- End Part 1 --