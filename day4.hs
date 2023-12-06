-- Authored by Sam Horovatin

import System.Environment
import System.IO 
import Data.List 
import Data.Maybe
import Data.Char

-- Start Part 1 --
-- Wrong: 18809
-- Wrong: 27840
-- Correct: 27845
{-
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle 
    let fileLines = lines contents
    print (sum (map (findCardValue 0 . splitKeysCards "") fileLines))
    hClose handle -- Remember to close files when done
-}
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

-- End Part 1 --

-- Start Part 2 --
-- Correct: 9496801
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode -- Neet way to open filenames passed in as args
    contents <- hGetContents handle 
    let fileLines = lines contents
    let cardCounts = (map (findCardValue2 . splitKeysCards "") fileLines) -- Sorry for the use of of an extra let :(
    print (findNumCards 0 ((replicate (length cardCounts) 1), cardCounts))
    hClose handle -- Remember to close files when done

-- Could have applied log base 2 to find findCardValue, but didn't feel like dealing with -Infinity for application on 0
-- This is clunkier, but it works (slightly modified findCardValue)
findCardValue2 :: ([Int], [Int]) -> Int
findCardValue2 keysAndCards = case keysAndCards of 
    (k:ks, c:cs) -> if k == c 
        then  1 + findCardValue2 (k:ks, cs)
        else if k < c then findCardValue2 (ks, c:cs)
        else if k > c then findCardValue2 (k:ks, cs)
        else findCardValue2 (ks, cs)
    (_, _) -> 0

-- Takes an 0 inited accumulator, a tuple of (number of cards per game, winning per game) and generates the number of cards
findNumCards :: Int -> ([Int], [Int])-> Int
findNumCards cardAcc (numCards, cardWinnings) = case (numCards, cardWinnings) of
    -- adds current num of cards (c) to acc, then adds the current number of cards (c) to the number of winnings (w) cards ahead
    -- map (+c) (take w cs)++drop w cs is just a way to update w elements of the number of cards list with +c
    (c:cs, w:ws) -> findNumCards (cardAcc+c) (map (+c) (take w cs)++drop w cs, ws) 
    (_, _) -> cardAcc

-- End Part 2 --

