-- Authored by Sam Horovatin

import System.Environment
import System.IO 
import Text.Read (readMaybe)

-- Start Part 1 --

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode 
    contents <- hGetContents handle 
    let filelines = words contents
    let ns = [("one",1::Int), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]
    let test = "slconeightfoureight557m38"
    --print (wordsToDigit ns test)
    print (sum (map (gen2DigitNum . wordsToDigit ns) filelines))
    hClose handle

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

-- Start Part 2 --
wordsToDigit :: [(String, Int)] -> String -> [Int]
wordsToDigit ns xs = case xs of [] -> []
                                [x] -> case readMaybe [x] :: Maybe Int of Nothing -> []
                                                                          Just x' -> [x']
                                x:xs -> case readMaybe [x] :: Maybe Int of Nothing -> helperWordToDigit ns (x:xs) ++ wordsToDigit ns xs
                                                                           Just x'  -> x' : helperWordToDigit ns xs ++ wordsToDigit ns xs

helperWordToDigit :: [(String, Int)] -> String -> [Int]
helperWordToDigit ns xs = case ns of [] -> []
                                                -- Bool logical compare of xs and numbers string name, multiplied by numbers value
                                                -- (1 or 0) * (value of number) = (0) or (value of number)
                                     ns -> case fromEnum (take (length (fst (head ns))) xs == fst (head ns)) * snd (head ns) of 
                                        0 -> helperWordToDigit (tail ns) xs 
                                        x -> [x]
-- End Part 2 --