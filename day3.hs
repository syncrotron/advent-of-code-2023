-- Authored by Sam Horovatin

import System.Environment
import System.IO 
import Data.List 
import Text.Read ( readMaybe, Lexeme (String) )
import Data.Maybe
import Data.Char
import Data.ByteString.Char8 (splitWith)

-- Start Part 1 --
{-
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle 
    let fileLines = lines contents
    let listOfParts = map (groupBy groupHelper) (findPartNums (linesToTriples "" fileLines))
    print (sum (map (read :: String -> Int) (concatMap ( filter filterHelper) (filter containsSys listOfParts))))
    hClose handle -- Remember to close files when done
-}


zipTriple :: (String, String, String) -> [(Char, Char, Char)]
zipTriple trip = case trip of 
    (_, [], _) -> []
    (xs, ys, []) -> zipWith (curry tripler) (zip xs ys) (replicate (length ys) '.') 
    ([], ys, zs) -> zipWith (curry tripler) (zip (replicate (length ys) '.') ys) zs
    (xs, ys, zs) -> zipWith (curry tripler) (zip xs ys) zs 
    where tripler ((x, y), z) = (x, y, z)

-- Inputs: an empty accumulator String and the list of lines (Strings) 
-- Outputs: a triple of adjacent chars from the selected center (above, center, below) 
linesToTriples :: String -> [String] -> [[(Char, Char, Char)]]
linesToTriples prevLine lines = case lines of 
    [] -> []
    [y] -> [zipTriple (prevLine, y, "")] -- Handles bottom row of string
    y:z:ys -> zipTriple (prevLine, y, z) : linesToTriples y (z:ys)

findPartNums :: [[(Char, Char, Char)]] -> [String]
findPartNums tripLines = case tripLines of
    [] -> []
    l:ls -> case l of
        [] -> []
        l ->  findPartNumsHelper "" l ++ findPartNums ls

findPartNumsHelper :: String -> [(Char, Char, Char)] -> [String]
findPartNumsHelper strAcc line = case line of
    [] -> []
    [(x, y, z)] -> [getSym (x, y, z) ++ strAcc ++ stringIfNum y]
    (x, y, z):ls -> if (isSym x || isSym y || isSym z) && strAcc == ""
    then findPartNumsHelper (getSym (x, y, z) ++ strAcc ++ stringIfNum y) ls 
    else if isNumber y then
        findPartNumsHelper (getSym (x, y, z) ++ strAcc ++ [y]) ls
    else if isSym x || isSym y || isSym z then
        (getSym (x, y, z) ++ strAcc) : findPartNumsHelper (getSym (x, y, z)) ls
    else strAcc : findPartNumsHelper "" ls

-- Helper Functions --

groupHelper :: Char -> Char -> Bool
groupHelper x y = isNumber x == isNumber y

filterHelper :: String -> Bool
filterHelper x = case readMaybe x :: Maybe Int of
    Nothing -> False
    Just _ -> True

containsSys :: [String] -> Bool  
containsSys x = case x of 
    [] -> False
    x:xs -> any isSym x || containsSys xs

stringIfNum :: Char -> String
stringIfNum x = if isNumber x then [x] else ""

isSym :: Char -> Bool
isSym x =  not (isPeriod x) && not (isNumber x) 

isPeriod :: Char -> Bool
isPeriod x = x == '.'

getSym :: (Char, Char, Char) -> String
getSym (x, y, z)
    | isSym x = [x]
    | isSym z = [z]
    | isSym y = [y]
    | otherwise = ""

-- End Part 1 --

-- Start Part 2 --

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle 
    let test1 = "......755."
    let test2 = "...$.*...."
    let test3 = ".664.598.."
    let fileLines = lines contents
    let listOfParts = linesToTriples2 fileLines
    print (listOfParts)
    hClose handle -- Remember to close files when done

-- Verified
linesToTriples2 :: [String] -> [[(Char, Char, Char)]]
linesToTriples2 lines = case lines of 
    [] -> []
    [y] -> zipTriple (y, replicate (length y) '.', replicate (length y) '.') : linesToTriples2 [] -- Handles bottom row of string
    (y:z:z':ys) -> zipTriple (y, z, z') : linesToTriples2 (z':ys)
    (y:z:ys) -> zipTriple (y, z, replicate (length y) '.') : linesToTriples2 (z:ys) -- Handles second bottom row of string

-- findPartNums2 :: [[(Char, Char, Char)]] -> [(String, String)]
-- findPartNums2 tripLines = case tripLines of
--     [] -> []
--     l:ls -> case l of
--         [] -> []
--         l ->  findPartNumsHelper2 "" l ++ findPartNums2 ls

-- findPartNumsHelper2 :: String -> [(Char, Char, Char)] -> [(Char, Char, Char)] -> [(String, String)]
-- findPartNumsHelper2 strAcc line = case line of
--     [] -> []
--     (y, z, z'):ls -> if (isGear y || isGear z || isGear z') && strAcc == ""
--     then (findPartNumsHelper2 (strAcc ++ stringIfNum y) ls, ) 
--     else if isNumber y then
--         findPartNumsHelper2 (getSym ('.', y, z) ++ strAcc ++ [y]) ls
--     else if isSym y || isSym z then
--         (getSym ('.', y, z) ++ strAcc, "") : findPartNumsHelper2 (getSym ('.', y, z)) ls
--     else strAcc : findPartNumsHelper "" ls

isGear :: Char -> Bool
isGear x = x == '*'

travelThroughGear :: String -> [(Char, Char, Char)] -> [(Char, Char, Char)] -> String
travelThroughGear strAcc origCol curCol = case curCol of
    [] -> []
    (y, z, z'):ls -> if isGear y 
        then travelThroughGear strAcc origCol (drop (length origCol - length ls - 1) origCol) 
        ++ travelThroughGear strAcc origCol (drop (length origCol - length ls +1) origCol)
        else if isGear z then travelThroughGear strAcc origCol (tail (drop (length ((y, z, z'):ls)) origCol))
        else if isGear z' then travelThroughGear strAcc origCol (tail (drop (length ((y, z, z'):ls) + 1) origCol)) 
        else []

-- End Part 2 --