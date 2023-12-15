-- Authored by Sam Horovatin
import System.Environment
import System.IO 
import Data.List 
import Data.Maybe
import Data.Char

-- Start Part 1 --
-- Wrong: 
-- Correct: 170000

-- main = do
--     args <- getArgs
--     handle <- openFile (head args) ReadMode
--     contents <- hGetContents handle
--     -- All "lets" done for readability. Could be condensed into final call in brackets
--     let fileLines = lines contents
--     let intFileLines = map (map (read :: String -> Float) . tail . words) fileLines
--     print (product (zipWith (curry quadraticSolution) (intFileLines !! 0) (intFileLines !! 1)))
--     hClose handle -- Remember to close files when done

-- -x^2 + xa - y = 0, x = speed in mm/s, y = distance (mm) travelled, a = Total race time (s)
-- Take a tuple of (a, y), returns number of whole second solutions above record distance y
quadraticSolution :: (Float, Float) -> Int
quadraticSolution (a, y) =  (floor startX) - (floor endX)
    where startX = (-a - sqrt (a*a - (4*(y+0.1)))) / (-2) -- Need to add small amount (.1) to y so I am not getting exact intercepts of record distance
          endX = (-a + sqrt (a*a - (4*(y+0.1)))) / (-2)

-- End Part 1 --

-- Start Part 2 --
-- Wrong: 20537784, ↑
-- Correct: 20537782

-- -x^2 + xa - y = 0, x = speed in mm/s, y = distance (mm) travelled, a = Total race time (s)
-- Take a tuple of (a, y), returns number of whole second solutions above record distance y
-- Needs precision of double for very large numbers 
quadraticSolutionDouble :: (Double, Double) -> Int
quadraticSolutionDouble (a, y) = floor (startX - endX)
    where startX = (-a - sqrt (a*a - (4*(y+0.1)))) / (-2) -- Need to add small amount (.1) to y so I am not getting exact intercepts of record distance
          endX = (-a + sqrt (a*a - (4*(y+0.1)))) / (-2)


main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let fileLines = lines contents -- O(n)
    let intFileLines = map ((read :: String -> Double) . concat . tail . words) fileLines -- O(n)
    print (quadraticSolutionDouble (intFileLines !! 0 , intFileLines !! 1)) -- O(1)
    hClose handle -- Remember to close files when done

-- End Part 2 --