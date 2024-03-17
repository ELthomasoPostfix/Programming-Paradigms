import Data.List
import Data.Char



main = do
    contents <- readFile "day4.txt"
    putStrLn ("part 1: " ++ show (computeTotalValueP1 contents))
    putStrLn ("part 2: " ++ show (computeTotalValueP2 (lines contents) []))


--------------------
--     PART 1     --
--------------------


-- Compute the card value for a single input line that has its ':' prefix
-- remove. i.e. a line of the form "num1 .. numx | num1 .. numy".
-- Params:
--      1) The input line, stripped of the prefix up until the ':' character
computeCardValue :: String ->  Int
computeCardValue input = if overlap > 0 then value else 0
                         where overlap = computeCardMatchAmnt input
                               value = 2 ^ (overlap - 1)

-- Compute and add the card value per card.
-- Params:
--      1) The complete file input
computeTotalValueP1 :: String -> Int
computeTotalValueP1 "" = 0
computeTotalValueP1 input = sum (map (\line -> computeCardValue (stripLinePrefix line)) (lines input))


--------------------
--     PART 2     --
--------------------


-- Parse a String representing list of space-separated
-- numbers into an actual list of the corresponding Int values.
-- Params:
--      1) The string representing a list of space-separated Ints
parseNumList :: String -> [Int]
parseNumList str = foldl (\lst word -> ((read word::Int) : lst)) [] (words str)

-- Split the string once on the given character, and return both parts.
-- Params:
--      1) The char to split on
--      2) The string to split into two parts
splitOnce :: Char -> String -> (String, String)
splitOnce char (head:xs)
    | head == char = ("", xs)
    | otherwise    = (head:left, right)
    where (left, right) = splitOnce char xs

-- Compute the the size of the win-guess overlap for a single input line
-- that has its ':' prefix removed.
-- i.e. a line of the form "num1 .. numx | num1 .. numy".
-- Params:
--      1) The input line, stripped of the prefix up until the ':' character
computeCardMatchAmnt :: String -> Int
computeCardMatchAmnt numStr =
    length (intersect (parseNumList left) (parseNumList right))
    where (left, right) = splitOnce '|' numStr

-- Strip the "Card x:" prefix off the line and return the stripped line.
-- Params:
--      1) A line of input, which MUST have a prefix containing a ':'
stripLinePrefix :: String -> String
stripLinePrefix line = snd (splitOnce ':' line)

-- Create a list of specified length filled with a single Int value.
-- Params:
--      1) The length of the target list, clamped to [0, +inf]
--      2) The value to fill with
full :: Int -> Int -> [Int]
full len value = take (max 0 len) [value, value..]

-- Update the list of running card copy amounts.
-- e.g. Update given nr=2, value=7, amnts=[10]         -> [17, 7]
-- e.g. Update given nr=2, value=7, amnts=[10, 20]     -> [17, 27]
-- e.g. Update given nr=2, value=7, amnts=[10, 20, 30] -> [17, 27, 30]
-- Params:
--      1) The number of cards to generate copies for
--      2) The amount of copies to generate per card; the copy value
--      3) The list of copy amounts to update
updateCopyAmnts :: Int -> Int -> [Int] -> [Int]
updateCopyAmnts copyNr copyValue copyAmnts =
    zipWith (+) (copyAmnts ++ lstPadding) ((full copyNr copyValue) ++ valuePadding)
    where lstLen       = length copyAmnts
          lstPadding   = full (copyNr - length copyAmnts) 0
          valuePadding = full ((length copyAmnts) - copyNr) 0

-- Compute and add the card value per card.
-- Params:
--      1) The complete file input
--      2) Initialize to [], the running list of card copy amounts
computeTotalValueP2 :: [String] -> [Int] -> Int
computeTotalValueP2 [] copyAggregates = sum copyAggregates
computeTotalValueP2 (line:lxs) copyAggregates
    | copyAggregates == [] = 1 + computeTotalValueP2 lxs (updateCopyAmnts copyNr 1 [])
    | otherwise            = 1 + aggr + computeTotalValueP2 lxs (updateCopyAmnts copyNr (1+aggr) axs)
    where copyNr     = computeCardMatchAmnt (stripLinePrefix line)
          (aggr:axs) = copyAggregates
