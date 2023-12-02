-- Authored by Sam Horovatin

import System.Environment
import System.IO 
import Data.List 
import Text.Read ( readMaybe )
import Data.Maybe

-- Start Part 1 --
{-
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
-}

findGameID :: [String] -> Int
findGameID xs = fromMaybe 0 (readMaybe (init (head xs)))

evalGame :: Maybe Int -> [String] -> Bool
evalGame mi xs = case xs of [] -> True
                            -- removes pesky ',' and ';' chars for pattern matching
                            x:xs' -> case delete ',' (delete ';' x) of 
                                "red" -> (fromMaybe 0 mi <= 12) && evalGame Nothing xs' 
                                "green" -> (fromMaybe 0 mi <= 13) && evalGame Nothing xs' 
                                "blue" -> (fromMaybe 0 mi <= 14) && evalGame Nothing xs' 
                                _ -> evalGame (readMaybe x) xs' 

-- End Part 1 --

-- Start Part 2 --
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle 
    let fileLines = lines contents
    -- Trims "Game" and Game Number off front of list, then seperates words
    let trimedAndSegmentedLines = map (drop 2 . words) fileLines
    -- Takes trimmed list, finds the power for each game, and sums them together
    print (sum (map (evalGamePower 0 (0,0,0)) trimedAndSegmentedLines))
    hClose handle -- Remember to close files when done

-- A little sloppy to call due to 3 args, but gets the job done
evalGamePower :: Int -> (Int, Int, Int) -> [String] -> Int
evalGamePower rgb rgbAcc xs = case (xs, rgbAcc) of 
    ([], (r, g, b)) -> r * g * b
    (x:xs', (r, g, b)) -> case delete ',' (delete ';' x) of
        "red" -> evalGamePower 0 (max r rgb, g, b) xs'
        "green" -> evalGamePower 0 (r , max g rgb, b) xs'
        "blue" -> evalGamePower 0 ( r, g, max b rgb) xs'
        _ -> evalGamePower (read x) (r , g, b) xs' 


-- end Part 2 --