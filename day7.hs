-- Authored by Sam Horovatin


import System.Environment
import System.IO 
import Data.List 
import Data.Maybe
import Data.Char
import Text.Read ( readMaybe )
import Data.List (sort)

-- Start Part 1 --
-- Correct: 282277027


main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let fileLines = lines contents
    print (parsedSortedHands [] fileLines)
    hClose handle -- Remember to close files when done

data Cards = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show, Read, Bounded, Enum)  


parsedSortedHands :: [([Cards], Int)] -> [String] -> [([Cards], Int)]
parsedSortedHands sortedHands lines = case lines of 
    [] -> sortedHands
    l:ls -> parsedSortedHands updatedHand ls
        where cards = map charToCard (take 5 l)
              score = read (last (words l))
              updatedHand = insertSortHands sortedHands (cards, score)

insertSortHands :: [([Cards], Int)] -> ([Cards], Int) -> [([Cards], Int)]
insertSortHands handsList hand = case handsList of
    [] -> [hand]
    h:hs -> if listHandScore > inHandScore then hand:handsList
            else if listHandScore == inHandScore then (sortColResolver h hand h hand) ++ hs
            else h:insertSortHands hs hand ++ [h]
        where listHandScore = handScorer (fst h)
              inHandScore = handScorer (fst hand)

-- Dependant on 5 card hands 
sortColResolver :: ([Cards], Int) -> ([Cards], Int) -> ([Cards], Int) -> ([Cards], Int) -> [([Cards], Int)]
sortColResolver aHandOrg bHandOrg aHand bHand 
    | null aCards || null bCards = [(fst aHandOrg, aScore+bScore)] -- This means the cards are equal
    | (head aCards) > (head bCards) = aHandOrg:[bHandOrg]
    | (head aCards) < (head bCards) = bHandOrg:[aHandOrg]
    | otherwise = sortColResolver aHandOrg bHandOrg (tail aCards, aScore) (tail bCards, aScore)
    where aCards = fst aHand
          bCards = fst bHand
          aScore = snd aHand
          bScore = snd bHand

charToCard :: Char -> Cards
charToCard card = case card of
    '2' -> Two
    '3' -> Three
    '4' -> Four
    '5' -> Five
    '6' -> Six
    '7' -> Seven
    '8' -> Eight
    '9' -> Nine
    'T' -> Ten
    'J' -> Jack
    'Q' -> Queen
    'K' -> King
    'A' -> Ace
    _ -> error "Unknown Char encountered. No Card mapping"

-- Takes list of cards and scores them from 0-5 (Int)
handScorer :: [Cards] -> Int
handScorer cards
    -- Gaurd order is important
    | fiveOfKind cards = 6
    | fourOfKind cards = 5
    | fullHouse cards = 4
    | threeOfKind cards = 3
    | twoPair cards = 2
    | onePair cards = 1   
    | otherwise = 0

-- All following hand types are dependant on hands being 5 cards long
fiveOfKind :: [Cards] -> Bool
fiveOfKind cards = case (sort cards) of
    [a,_,_,_,e] -> a == e

fourOfKind :: [Cards] -> Bool
fourOfKind cards = case (sort cards) of
    [a,b,_,d,e] -> (a == d) || (b == e)

fullHouse :: [Cards] -> Bool
fullHouse cards = case (sort cards) of
    [a,b,c,d,e] -> (a == b) && (c == e) ||
                   (a == c) && (d == e) 

threeOfKind :: [Cards] -> Bool
threeOfKind cards = case (sort cards) of
    [a,b,c,d,e] -> (a == b) && (b == c) ||
                   (c == d) && (d == e)

twoPair :: [Cards] -> Bool
twoPair cards = case (sort cards) of
    [a,b,c,d,e] -> (a == b) && (c == d) || 
                   (a == b) && (d == e) ||
                   (b == c) && (d == e) 

onePair :: [Cards] -> Bool
onePair cards = case (sort cards) of
    [a,b,c,d,e] -> (a == b) || 
                   (b == c) || 
                   (c == d) || 
                   (d == e)

-- Would be redundant in 
highCard :: [Cards] -> Bool
highCard cards =  case (sort cards) of
    [a,b,c,d,e] -> (a /= b) || 
                   (b /= c) || 
                   (c /= d) || 
                   (d /= e)