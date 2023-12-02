-- Authored by Sam Horovatin

import System.Environment
import System.IO 
import Data.List 
import Text.Read ( readMaybe )
import Data.Maybe

-- Start Part 1 --

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle 
    let fileLines = lines contents
    -- Trims "Game" off front of list, then seperates words
    let trimedAndSegmentedLines = map (tail . words) fileLines
    -- Takes trimmed list, filters out invalid games, finds the remaining games id's, and sums them
    print (sum (map findGameID (filter (evalGame Nothing) trimedAndSegmentedLines)))
    hClose handle -- Remember to close files when done

findGameID :: [String] -> Int
findGameID xs = fromMaybe 0 (readMaybe (init (head xs)))

evalGame :: Maybe Int -> [String]  -> Bool
evalGame mi xs = case xs of [] -> True
                            -- removes pesky ',' and ';' chars for pattern matching
                            x:xs' -> case delete ',' (delete ';' x) of 
                                "red" -> (fromMaybe 0 mi <= 12) && evalGame Nothing xs' 
                                "green" -> (fromMaybe 0 mi <= 13) && evalGame Nothing xs' 
                                "blue" -> (fromMaybe 0 mi <= 14) && evalGame Nothing xs' 
                                _ -> evalGame (readMaybe x) xs' 

-- End Part 1 --