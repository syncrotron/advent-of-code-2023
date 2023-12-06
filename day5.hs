
import System.Environment
import System.IO 
import Data.List 
import Data.Maybe
import Data.Char
import Text.Read ( readMaybe )
import Data.List (sort)


-- Start Part 1 --

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    let seeds = getSeeds (head fileLines)
    let maps = tail (orgMapping [] (extractMapping [] (tail fileLines)))
    print (calcMap seeds (maps !! 0))
    -- print 
    hClose handle -- Remember to close files when done

-- Returns list of keys as Ints
-- value (-1) is considered an error in reading string -> int
getSeeds :: String -> [Int]
getSeeds rawSeeds = map (fromMaybe (-1) . readMaybe) (words (drop 7 rawSeeds))


-- Generates ordered list of int lists. Mappings are seperated by empty lists.
extractMapping :: [Int] -> [String] -> [[Int]]
extractMapping mapAcc rawMapping = case rawMapping of 
    m:ms -> if m /= "" && isNumber (head m)
        then map (fromMaybe (-1) . readMaybe) (words (head rawMapping)):extractMapping mapAcc ms
        else if m == "" then mapAcc:extractMapping mapAcc ms
        else extractMapping mapAcc ms
    [] -> []

orgMapping :: [[Int]] -> [[Int]] -> [[[Int]]]
orgMapping mapAcc maps = case maps of
    [] -> []
    m:ms -> if m == []
        then mapAcc:orgMapping [] ms
        else orgMapping (m:mapAcc) ms

calcMap :: [Int] -> [[Int]] -> [Int]
calcMap seeds maps = case (seeds, maps) of 
    ([], _) -> []
    (_, []) -> []
    -- m !! 0 is the consistent position of destination range start
    -- m !! 1 is the consistent position of source range start
    -- m !! 2 is the consistent position of range length
    (s:ss, m:ms) -> case elemIndex s (take (m !! 2) [(m !! 1)..]) of
        Nothing -> calcMap (s:ss) ms ++ calcMap ss ms
        Just x -> (take (m !! 2) [(m !! 1)..]!! x) : calcMap ss ms 

calcLocations :: [Int] -> [[[Int]]] -> [Int]
calcLocations seeds mapsOfMaps = case mapsOfMaps of
    [] -> seeds
    m:ms -> calcLocations (calcMap seeds m) ms