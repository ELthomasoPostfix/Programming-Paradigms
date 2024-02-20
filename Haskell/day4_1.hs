import Data.List
import Data.Char



main = do
    contents <- readFile "day4.txt"
    print (computeTotalValue contents)
    return ()


--------------------
--     PART 1     --
--------------------

-- Parse a String representing list of space-separated
-- numbers into an actual list of the corresponding Int values.
-- Params:
--      1) The string representing a list of space-separated Ints
--      2) A value carries through recursive calls, to keep track
--         of the current, incomplete Int substring
parseNumList :: String -> String -> [Int]
parseNumList "" "" = []
parseNumList "" numCarry = [read numCarry]
parseNumList (head:xs) numCarry
    | isDigit head   = parseNumList xs (numCarry ++ (head:""))
    | numCarry == "" = parseNumList xs numCarry
    | head == ' '    = (read numCarry) : (parseNumList xs "")

-- Compute the card value for a single input line that has its ':' prefix
-- remove. i.e. a line of the form "num1 .. numx | num1 .. numy".
-- Params:
--      1) The input line, stripped of the prefix up until the ':' character
--      2) All characters in the input left of the '|', as a String
--      3) All characters in the input right of the '|', as a String
--      4) A Bool that represents whether the '|' has been seen already in the input
computeCardValue :: String -> String -> String -> Bool -> Int
computeCardValue "" pipeLeft pipeRight _ = if resLen > 0
                                           then 2 ^ (resLen - 1)
                                           else 0
                                    where winNums = parseNumList pipeLeft ""
                                          gotNums = parseNumList pipeRight ""
                                          resLen  = length (intersect winNums gotNums)
computeCardValue (head:xs) pipeLeft pipeRight pipeSeen
    | head == '|' = computeCardValue xs pipeLeft pipeRight True
    | pipeSeen    = computeCardValue xs pipeLeft (pipeRight ++ (head:"")) pipeSeen
    | otherwise   = computeCardValue xs (pipeLeft ++ (head:"")) pipeRight pipeSeen

-- Strip the "Card x:" prefix off the line.
-- Params:
--      1) A line of input, which MUST have a prefix containing a ':'
stripLinePrefix :: String -> String
stripLinePrefix (head:xs)
    | head == ':' = xs
    | otherwise   = stripLinePrefix xs

-- Compute and add the card value per card.
-- Params:
--      1) The complete file input
computeTotalValue :: String -> Int
computeTotalValue "" = 0
computeTotalValue input = sum (map (\line -> computeCardValue (stripLinePrefix line) "" "" False) (lines input))