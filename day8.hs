-- Authored by Sam Horovatin


import System.Environment
import System.IO 
import Data.List 
import Data.Maybe (fromJust)
import Data.Char
import Data.Map (Map) -- O(log n), basically balanced binary trees
import Data.Map (lookup, findWithDefault, keys)
import qualified Data.Map as Map 
import Debug.Trace


-- Start Part 1 --
-- Wrong: , ↑↓
-- Correct: 16579

-- main = do
--     args <- getArgs
--     handle <- openFile (head args) ReadMode
--     contents <- hGetContents handle
--     -- All "lets" done for readability. Could be condensed into final call in brackets
--     let fileLines = lines contents -- O(n)
--     let directions = head fileLines -- Ain't pretty but it works
--     let dirMap = strToMap (drop 2 fileLines) -- Drops the line of L's and R's and the blank line after it
--     print (mapWonder dirMap 0 ("AAA") directions ) -- Given the AAA as common start for all maps
--     hClose handle -- Remember to close files when done

turn :: Char -> (a, a) -> a
turn x = if x == 'L' then fst else snd

-- Takes a Map (Key: String, Val: (String, String)), a step counter, a key, and the string containing the L's and R's
-- Returns the number of steps to get to ZZZ
mapWonder :: Map String (String,String) -> Int -> String -> String -> Int
mapWonder dirMap steps curKey dirs = case (curKey, dirs) of
   ("", _) -> steps
   ("ZZZ", _) -> steps
   (curKey, x:xs) -> mapWonder dirMap (steps+1) (turn x (findWithDefault ("","") curKey dirMap)) (xs ++ [x]) -- O(log n)

-- Input Parsing to break out string input into Map (Key: String, Val: (String, String))
strToMap :: [String] -> Map String (String,String)
strToMap rawMaps = Map.fromList (zip keys values)
    where keys = map (take 3) rawMaps
          values = zip (map ((take 3) . (drop 7)) rawMaps) (map ((take 3) . (drop 12)) rawMaps)

-- End Part 1 --

-- Start Part 2 --
-- Wrong: , ↑↓
-- Correct: 

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let fileLines = lines contents -- O(n)
    let directions = head fileLines -- Ain't pretty but it works
    let dirMap = strToMap (drop 2 fileLines) -- Drops the line of L's and R's and the blank line after it
    print (mapGhostStep 0 dirMap ((collectStarts dirMap)) directions)

    hClose handle -- Remember to close files when done

mapGhostStep :: Int -> Map String (String,String) -> [String] -> String -> Int
mapGhostStep steps dirMap keys dirs = case (filter (\x -> case x of [_,_,'Z'] -> False; _ -> True) keys) of
    [] -> steps
    _ -> case dirs of 
            x:xs -> mapGhostStep (steps+1) dirMap (map (helper) keys) (xs ++ [x])
                where helper y = turn x (fromJust (Data.Map.lookup y dirMap)) -- Used fromJust so it would throw error if it encounteded missing key
            _ -> steps

collectStarts :: Map String (String,String) -> [String]
collectStarts dirMap = filter (\x -> case x of [_,_,'A'] -> True; _ -> False) (keys dirMap)
-- End Part 2 --