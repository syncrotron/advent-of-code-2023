-- Authored by Sam Horovatin


import System.Environment
import System.IO 
import Data.List 
import Data.Maybe
import Data.Char
import Text.Read ( readMaybe )
import Data.List (sort)
import Data.List (sortOn)
import Data.Function (on)


-- Start Part 1 --
-- Wrong: 244282690, ↑
-- Correct: 241344943

-- main = do
--     args <- getArgs
--     handle <- openFile (head args) ReadMode
--     contents <- hGetContents handle
--     -- All "lets" done for readability. Could be condensed into final call in brackets
--     let fileLines = lines contents
--     let sortedHands = sortBy (handCompare) (fileLines) --Supposedly, sortBy is O(n log(n))
--     let sortedScores = map (drop 6) sortedHands
--     print (multiplyByIndex 1 (map (read :: String -> Int) sortedScores))
--     print (sum (multiplyByIndex 1 (map (read :: String -> Int) sortedScores)))
--     hClose handle -- Remember to close files when done

multiplyByIndex :: Int -> [Int] -> [Int]
multiplyByIndex mutiplyer iList = case iList of
    [] -> []
    i:is -> i*mutiplyer : multiplyByIndex (mutiplyer+1) is

-- Rule 1 and Rule 2 Boolean check for if handA < handB
handCompare :: String -> String -> Ordering
handCompare handA handB = case (handA, handB) of
    (handA, handB) -> if (handAScore < handBScore) then LT
        else if (handAScore == handBScore) then cardCompare (take 5 handA) (take 5 handB)
        else GT
            where handAScore = handScorer handA
                  handBScore = handScorer handB 

-- Rule 2 Boolean check for if handA < handB
cardCompare :: String -> String -> Ordering
cardCompare handA handB = case (handA, handB) of
    (a:as, b:bs) -> if (cardAVal < cardBVal) then LT 
        else if (cardAVal > cardBVal) then GT 
        else cardCompare as bs
        where cardAVal = charToCardInt a
              cardBVal = charToCardInt b
    (handA, []) -> LT
    ([], handB) -> GT

charToCardInt :: Char -> Int
charToCardInt card = case card of
    '2' -> (1::Int)
    '3' -> 2
    '4' -> 3
    '5' -> 4
    '6' -> 5
    '7' -> 6
    '8' -> 7
    '9' -> 8
    'T' -> 9
    'J' -> 10
    'Q' -> 11
    'K' -> 12
    'A' -> 13
    _ -> error "Unknown Char encountered. No Card mapping"

-- Takes list of cards and scores them from 0-5 (Int)
handScorer :: String -> Int
handScorer cards
    -- Gaurd order is important
    | fiveOfKind hand = 7
    | fourOfKind hand = 6
    | fullHouse hand = 5
    | threeOfKind hand = 4
    | twoPair hand = 3
    | onePair hand = 2   
    | otherwise = 1
    where hand = (take 5 cards)

-- All following hand types are dependant on hands being 5 cards long
fiveOfKind :: String -> Bool
fiveOfKind cards = case (sort cards) of
    [a,_,_,_,e] -> a == e

fourOfKind :: String -> Bool
fourOfKind cards = case (sort cards) of
    [a,b,_,d,e] -> (a == d) || (b == e)

fullHouse :: String -> Bool
fullHouse cards = case (sort cards) of
    [a,b,c,d,e] -> ((a == b) && (b == c) && (d == e)) ||
                   ((a == b) && (c == d) && (d == e))

threeOfKind :: String -> Bool
threeOfKind cards = case (sort cards) of
    [a,b,c,d,e] -> ((a == b) && (b == c)) ||
                   ((b == c) && (c == d)) ||
                   ((c == d) && (d == e))

twoPair :: String -> Bool
twoPair cards = case (sort cards) of
    [a,b,c,d,e] -> ((a == b) && (c == d)) || 
                   ((a == b) && (d == e)) ||
                   ((b == c) && (d == e))

onePair :: String -> Bool
onePair cards = case (sort cards) of
    [a,b,c,d,e] -> (a == b) || 
                   (b == c) || 
                   (c == d) || 
                   (d == e)

-- Would be redundant in 
highCard :: String -> Bool
highCard cards =  case (sort cards) of
    [a,b,c,d,e] -> (a /= b) && 
                   (b /= c) && 
                   (c /= d) && 
                   (d /= e)

-- End Part 1 --

-- Start Part 2 --
-- Wrong: 242125746, ↓
-- Wrong: 242075418, ↓
-- Wrong: 243356816, ↑
-- Correct: 243101568

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let fileLines = lines contents
    let sortedHands = sortBy (handCompare2) (fileLines) --Supposedly, sortBy is O(n log(n))
    let sortedScores = map (drop 6) sortedHands
    print (sum (multiplyByIndex 1 (map (read :: String -> Int) sortedScores)))
    hClose handle -- Remember to close files when done

-- Rule 1 and Rule 2 Boolean check for if handA < handB
handCompare2 :: String -> String -> Ordering
handCompare2 handA handB = case (handA, handB) of
    (handA, handB) -> if (handAScore < handBScore) then LT
        else if (handAScore == handBScore) then cardCompare2 (take 5 handA) (take 5 handB)
        else GT
            where handAScore = handScorer2 handA
                  handBScore = handScorer2 handB 

-- Rule 2 Boolean check for if handA < handB
cardCompare2 :: String -> String -> Ordering
cardCompare2 handA handB = case (handA, handB) of
    (a:as, b:bs) -> if (cardAVal < cardBVal) then LT 
        else if (cardAVal > cardBVal) then GT 
        else cardCompare2 as bs
        where cardAVal = charToCardInt2 a
              cardBVal = charToCardInt2 b
    (handA, []) -> LT
    ([], handB) -> GT

charToCardInt2 :: Char -> Int
charToCardInt2 card = case card of
    '2' -> (1::Int)
    '3' -> 2
    '4' -> 3
    '5' -> 4
    '6' -> 5
    '7' -> 6
    '8' -> 7
    '9' -> 8
    'T' -> 9
    'J' -> -1 -- Not great solution, but it works. Deals with low joker value
    'Q' -> 11
    'K' -> 12
    'A' -> 13
    _ -> error "Unknown Char encountered. No Card mapping"

-- Sorry for the following code replication; trying for speed to catch up
handScorer2 :: String -> Int
handScorer2 cards
    -- Gaurd order is important
    | fiveOfKind2 hand = 7
    | fourOfKind2 hand = 6
    | fullHouse2 hand = 5
    | threeOfKind2 hand = 4
    | twoPair2 hand = 3
    | onePair2 hand = 2   
    | otherwise = 1
    where hand = (take 5 cards)

-- All following hand types are dependant on hands being 5 cards long
-- All modified to include jokers as acceptable hands 
fiveOfKind2 :: String -> Bool
fiveOfKind2 cards = case (jokerSort cards) of
    [_,'J','J','J','J'] -> True
    [a,b,'J','J','J'] -> a == b
    [a,_,c,'J','J'] -> a == c
    [a,_,_,d,'J'] -> a == d
    [a,_,_,_,e] -> a == e 

fourOfKind2 :: String -> Bool
fourOfKind2 cards = case (jokerSort cards) of
    [_,_,'J','J','J'] -> True
    [a,b,c,'J','J'] -> (a == b) || (a == c) || (b == c)
    [a,b,c,d,'J'] -> (a == d) || (a == c) || (b == d)
    [a,b,_,d,e] -> (a == d) || (b == e)

fullHouse2 :: String -> Bool
fullHouse2 cards = case (jokerSort cards) of
    [_,_,'J','J','J'] -> True
    [a,b,c,'J','J'] -> (a == b) || (a == c) || (b == c)
    [a,b,c,d,'J'] -> ((a == b) && (b == c)) || 
                     ((b == c) && (c == d)) ||
                     ((a == b) && (c == d))
    [a,b,c,d,e] -> ((a == b) && (b == c) && (d == e)) ||
                   ((a == b) && (c == d) && (d == e))

threeOfKind2 :: String -> Bool
threeOfKind2 cards = case (jokerSort cards) of
    [_,_,_,'J','J'] -> True
    [a,b,c,d,'J'] -> (a == b) || (b == c) || (c == d)
    [a,b,c,d,e] -> ((a == b) && (b == c)) ||
                   ((b == c) && (c == d)) ||
                   ((c == d) && (d == e))

twoPair2 :: String -> Bool
twoPair2 cards = case (jokerSort cards) of
    [_,_,_,'J','J'] -> True
    [a,b,c,d,'J'] -> (a == b) || (b == c) || (c == d) 
    [a,b,c,d,e] -> ((a == b) && (c == d)) || 
                   ((a == b) && (d == e)) ||
                   ((b == c) && (d == e))

onePair2 :: String -> Bool
onePair2 cards = case (jokerSort cards) of
    [_,_,_,_,'J'] -> True
    [a,b,c,d,e] -> (a == b) || 
                   (b == c) || 
                   (c == d) || 
                   (d == e)

-- Custom string sort, which moves all joker cards to end of hand
jokerSort :: String -> String
jokerSort = sortBy jokerSortHelper

-- This is a mess. Used by jockerSort as sortBy input function. 
-- Essentially, it calculates the follow
--  b == J, b Less Than a
--  a == J, a Less Than b
--  a > b, b Less Than a
--  a < b, a Less Than b
--  a == b, a Equals b
jokerSortHelper :: Char -> Char -> Ordering
jokerSortHelper a b 
    | b == 'J' = LT
    | a == 'J' = GT
    | a > b = GT
    | a < b = LT
    | a == a = EQ
