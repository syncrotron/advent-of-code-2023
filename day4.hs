-- Authored by Sam Horovatin
-- Wrong: 18809
-- Wrong: 27840
-- Correct: 27845
import System.Environment
import System.IO 
import Data.List 
import Text.Read ( readMaybe, Lexeme (String) )
import Data.Maybe
import Data.Char
import Data.ByteString.Char8 (splitWith)

-- Start Part 1 --

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle 
    let fileLines = lines contents
    let listOfParts = fileLines 
    print (sum (map (findCardValue 0 . splitKeysCards "") listOfParts))
    hClose handle -- Remember to close files when done

-- Returns list of sorted int tuples, with the following structure (keys, cards)
-- Should be broken up into more functions, but it was faster and easier this way
splitKeysCards :: String -> String -> ([Int], [Int])
splitKeysCards winNumAcc card = case card of 
    "" -> ([],[])
    l:ls -> case l of 
        ':' -> splitKeysCards "" ls
        '|' -> (sort (map (read :: String -> Int) (words winNumAcc)), sort (map (read :: String -> Int) (words ls)))
        _ -> splitKeysCards (winNumAcc ++ [l]) ls

-- Takes an empty int score accumulator and list of (keys, cards), returns the total value of the card
findCardValue :: Int -> ([Int], [Int]) -> Int
findCardValue valAcc keysAndCards = case keysAndCards of 
    (k:ks, c:cs) -> if k == c 
        then findCardValue (max 1 (valAcc + valAcc)) (k:ks, cs)
        else if k < c then findCardValue valAcc (ks, c:cs)
        else if k > c then findCardValue valAcc (k:ks, cs)
        else findCardValue valAcc (ks, cs)
    (_, _) -> valAcc