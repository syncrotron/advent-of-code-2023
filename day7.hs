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
-- Correct: 

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let fileLines = lines contents
    let sortedHands = sortBy (handCompare) (fileLines) --Supposedly, sortBy is O(n log(n))
    let sortedScores = map (drop 6) sortedHands
    print sortedHands
    print (sum (multiplyByIndex 1 (map (read :: String -> Int) sortedScores)))
    hClose handle -- Remember to close files when done

multiplyByIndex :: Int -> [Int] -> [Int]
multiplyByIndex mutiplyer iList = case iList of
    [] -> []
    i:is -> i*mutiplyer : multiplyByIndex (mutiplyer+1) is

-- Rule 1 and Rule 2 Boolean check for if handA < handB
handCompare :: String -> String -> Ordering
handCompare handA handB = case (handA, handB) of
    (handA, handB) -> if (handAScore < handBScore) then LT
        else if (handAScore == handBScore) then cardComapre (take 5 handA) (take 5 handB)
        else GT
            where handAScore = handScorer handA
                  handBScore = handScorer handB 

-- Rule 2 Boolean check for if handA < handB
cardComapre :: String -> String -> Ordering
cardComapre handA handB = case (handA, handB) of
    (a:as, b:bs) -> if (cardAVal < cardBVal) then LT 
        else if (cardAVal > cardBVal) then GT 
        else cardComapre as bs
        where cardAVal = charToCardStr a
              cardBVal = charToCardStr b
    (handA, []) -> LT
    ([], handB) -> GT

charToCardStr :: Char -> Int
charToCardStr card = case card of
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
    [a,b,c,d,e] -> (a == b) && (c == e) ||
                   (a == c) && (d == e) 

threeOfKind :: String -> Bool
threeOfKind cards = case (sort cards) of
    [a,b,c,d,e] -> (a == b) && (b == c) ||
                   (c == d) && (d == e)

twoPair :: String -> Bool
twoPair cards = case (sort cards) of
    [a,b,c,d,e] -> (a == b) && (c == d) || 
                   (a == b) && (d == e) ||
                   (b == c) && (d == e) 

onePair :: String -> Bool
onePair cards = case (sort cards) of
    [a,b,c,d,e] -> (a == b) || 
                   (b == c) || 
                   (c == d) || 
                   (d == e)

-- Would be redundant in 
highCard :: String -> Bool
highCard cards =  case (sort cards) of
    [a,b,c,d,e] -> (a /= b) || 
                   (b /= c) || 
                   (c /= d) || 
                   (d /= e)