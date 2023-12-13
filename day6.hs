-- Authored by Sam Horovatin
import System.Environment
import System.IO 
import Data.List 
import Data.Maybe
import Data.Char

-- Start Part 1 --
-- Wrong: 
-- Correct: 

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    -- All "lets" done for readability. Could be condensed into final call in brackets
    let fileLines = lines contents
    let intFileLines = map (map (read :: String -> Float) . tail . words) fileLines
    print (product (zipWith (curry quadraticSolution) (intFileLines !! 0) (intFileLines !! 1)))
    hClose handle -- Remember to close files when done

-- -x^2 + xa - y = 0, x = speed in mm/s, y = distance (mm) travelled, a = Total race time (s)
-- Take a tuple of (a, y), returns number of whole second solutions aboev record distance y
quadraticSolution :: (Float, Float) -> Int
quadraticSolution (a, y) =  (floor startX) - (floor endX)
    where startX = (-a - sqrt (a*a - (4*(y+0.1)))) / (-2) -- Need to add small amount (.1) to y so I am not getting exact intercepts of record distance
          endX = (-a + sqrt (a*a - (4*(y+0.1)))) / (-2)

-- End Part 1 --