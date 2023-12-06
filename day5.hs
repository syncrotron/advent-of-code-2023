
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
    let maps = (extractMapping [] (tail fileLines))
    print maps
    print ""
    --print (map (calcLocations seeds) maps)
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
    [] -> []

calcLocations :: [Int] -> [[[Int]]] -> [Int]
calcLocations seeds mapsOfMaps = case mapsOfMaps of
    [] -> seeds
    m:ms -> calcLocations (calcMap seeds m) ms

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
