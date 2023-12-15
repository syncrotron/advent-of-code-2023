-- Authored by Sam Horovatin

import System.Environment
import System.IO 
import Data.List 
import Text.Read ( readMaybe, Lexeme (String) )
import Data.Maybe
import Data.Char

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
    let test0 = "......#..."
    let test1 = "*.....755."
    let test2 = "...$.*...."
    let test3 = ".664.598.."
    let fileLines = lines contents
    let listOfParts = linesToTriples2 [test0, test1, test2, test3]
    print (findGears (0,0) listOfParts)
    hClose handle -- Remember to close files when done

-- Verified
-- Takes in list of input strings, generates list of lists of line triples
-- where [(x, y, z)] is x = current row, y is x + 1 line chars, z is x + 2 line chars.
linesToTriples2 :: [String] -> [[(Char, Char, Char)]]
linesToTriples2 lines = case lines of 
    [] -> []
    [x] -> zipTriple (x, replicate (length x) '.', replicate (length x) '.') : linesToTriples2 [] -- Handles bottom row of string
    (x:y:z:zs) -> zipTriple (x, y, z) : linesToTriples2 (y:z:zs)
    (x:y:ys) -> zipTriple (x, y, replicate (length x) '.') : linesToTriples2 (y:ys) -- Handles second bottom row of string

-- findPartNums2 :: [[(Char, Char, Char)]] -> [(String, String)]
-- findPartNums2 tripLines = case tripLines of
--     [] -> []
--     l:ls -> case l of
--         [] -> []
--         l ->  findPartNumsHelper2 "" l ++ findPartNums2 ls

-- findPartNumsHelper2 :: String -> [(Char, Char, Char)] -> [(Char, Char, Char)] -> [(String, String)]
-- --should call travelThroughGears somewhere in here
-- findPartNumsHelper2 strAcc line = case line of
--     [] -> []
--     (y, z, z'):ls -> if (isGear y || isGear z || isGear z') && strAcc == ""
--     then (findPartNumsHelper2 (strAcc ++ stringIfNum y) ls, ) 
--     else if isNumber y then
--         findPartNumsHelper2 (getSym ('.', y, z) ++ strAcc ++ [y]) ls
--     else if isSym y || isSym z then
--         (getSym ('.', y, z) ++ strAcc, "") : findPartNumsHelper2 (getSym ('.', y, z)) ls
--     else strAcc : findPartNumsHelper "" ls

-- The idea is to position the gear in the y position of (x, y, z), then search for numbers recursively
-- in all directions. That is why I pass in origCol and do all the funky index stuff, as I am trying to
-- Allow crawls through the original matrix in all directions (except above y, as we assume we read all numbers top left down)
findGears :: (Int, Int) -> [[(Char, Char, Char)]] -> (Int, Int)
findGears gearIndex matrix = case matrix of
    [] -> gearIndex
    _ -> case (head matrix) of
        ('*', y, z):ls -> findGears (fst gearIndex, snd gearIndex + 1) ((tail matrix)) -- Gear is not in y, move down line 
        (x, '*', z):ls -> gearIndex
        _:ls -> findGears (fst gearIndex + 1, snd gearIndex ) (ls:(tail matrix))
        [] -> gearIndex

-- End Part 2 --