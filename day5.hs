
import System.Environment
import System.IO 
import Data.List 
import Data.Maybe
import Data.Char
import Text.Read ( readMaybe )
import Data.List (sort)


-- Start Part 1 --
-- Correct: 282277027
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let maps = extractMapping [] (tail fileLines) 
    let seeds = getSeeds (head fileLines)
    print (minimum (calcLocations seeds maps))
    
    hClose handle -- Remember to close files when done

-- Returns list of keys as Ints
-- value (-1) is considered an error in reading string -> int
getSeeds :: String -> [Int]
getSeeds rawSeeds = map (fromMaybe (-1) . readMaybe) (words (drop 7 rawSeeds))

-- Generates ordered list of lisst of int lists: 
--  Outer list reps all mappings; 
--  First order list reps specific maps (ie seed-to-soil); 
--  Second order list reps specific map.
extractMapping :: [[Int]] -> [String] -> [[[Int]]]
extractMapping mapAcc rawMapping = case rawMapping of 
    m:ms -> if m /= "" && isNumber (head m)
        then extractMapping (mapAcc ++ [map (fromMaybe (-1) . readMaybe) (words m)]) ms
        else if m == "" && not (null mapAcc) then mapAcc:extractMapping [] ms
        else if m == "" then extractMapping [] ms
        else extractMapping mapAcc ms
    [] -> [mapAcc]

-- Takes a list of seeds and a list of lists of lists of ints (the structured maps)
-- Returns a list of the locations
calcLocations :: [Int] -> [[[Int]]] -> [Int]
calcLocations seeds mapsOfMaps = case mapsOfMaps of
    [] -> seeds
    m:ms -> calcLocations (map (calcMap m) seeds) ms


-- Takes a list of lists (single group of x-x mapping) and a single seed Int
-- Retuns that seed mapped given the mapping
calcMap :: [[Int]] -> Int -> Int
calcMap maps seed = case maps of 
    -- m !! 0 is the consistent position of destination range start
    -- m !! 1 is the consistent position of source range start
    -- m !! 2 is the consistent position of range length
    m:ms -> if (seed >= m !! 1) && (seed <= (m !! 1)+(m !! 2)-1) 
        then (seed-(m !! 1))+(m !! 0)
        else calcMap ms seed
    [] -> seed

-- End Part 1 --