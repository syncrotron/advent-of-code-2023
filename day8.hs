-- Authored by Sam Horovatin


import System.Environment
import System.IO 
import Data.List 
import Data.Maybe
import Data.Char



-- Start Part1 --
-- Wrong: , ↑↓
-- Correct: 

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let fileLines = lines contents -- O(n)
    print (fileLines) -- O(1)
    hClose handle -- Remember to close files when done

    -- End Part 1 --