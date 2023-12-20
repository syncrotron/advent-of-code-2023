-- Authored by Sam Horovatin


import System.Environment
import System.IO 
import Data.List 
import Data.Maybe (fromJust)
import Data.Char
import Debug.Trace

-- Start Part 1 --
-- Wrong: , ↑↓
-- Correct: 1725987467

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let fileLines = lines contents -- O(n)
    let fileNums = map (map (read :: String -> Int) . words) fileLines
    print (sum (map (pyramidGenerator . (:[])) fileNums)) 
    hClose handle -- Remember to close files when done

pyramidGenerator :: [[Int]] -> Int
pyramidGenerator lists = if all (==0) (last lists) 
    then extrapolator 0 (reverse lists)
    else pyramidGenerator (lists ++ [diff (last lists)])

-- Inspired by https://stackoverflow.com/questions/20527756/comparing-list-elements-in-haskell 
diff :: [Int] -> [Int]
diff xs = map (* (-1)) (zipWith (-) (xs) $ (drop 1 xs))

extrapolator :: Int -> [[Int]] -> Int
extrapolator seed lists = case lists of 
    [] -> seed
    x:xs -> extrapolator (seed + (last x)) xs

-- End Part 1 --