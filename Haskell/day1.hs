import Data.Char
import Data.List
import Data.Maybe



main = do
    contents <- readFile "day1.txt"
    -- Part 1: Compute the calibration value aggregate
    let calibVals = extractCalibrationValues contents False
        result = sum calibVals
        in putStrLn ("part 1: " ++ (show result))

    -- Part 2: Compute the calibration value aggregate
    let calibVals = extractCalibrationValues contents True
        result = sum calibVals
        in putStrLn ("part 2: " ++ (show result))


-------------------------
--     PARTS 1 & 2     --
-------------------------


-- Check if the given Char is a numerich Char
isNumericChar :: Char -> Bool
isNumericChar c = elem c ['0'..'9']

-- If the given line is prefixed by a digit word OR the reverse of a digit word,
-- then return that prefix as an Int.
-- Number words are simply the digits one through nine fully spelled out as words.
-- i.e. If the line is "eightABCDE" then 8 is returned.
-- Else defaults to 0, since "zero" is not considered a number word for this use case.
-- Params:
--      1) The given line to extract the prefix of
--
-- https://stackoverflow.com/a/4941529
numwordPrefixAsInt :: String -> Int
numwordPrefixAsInt line
    | maybeIdx == Nothing = 0
    | otherwise           = idx + 1
    where maybeIdx = findIndex
                        -- Take the prefix of line of the same length as the number word.
                        -- The prefix must equal the numword or its reverse
                        (\numWord -> let prefix = (take (length numWord) line) in
                                            prefix == numWord || prefix == (reverse numWord))
                        -- The number words do not include "zero" by design.
                        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
          (Just idx) = maybeIdx

-- Retrieve the first numeric Char OR numeric Char word form, iterated from the front,
-- as an Int. If there is not such Char or word form, then return 0 by default.
-- Params:
--      1) The line for which to find the first Int
--      2) Whether to consider number words
firstNumericSubstrFromFront :: String -> Bool -> Int
firstNumericSubstrFromFront "" _ = 0
firstNumericSubstrFromFront str@(head:xs) includeWords
    | isNumericChar head      = read (head:"") :: Int
    | includeWords && num > 0 = num
    | otherwise               = firstNumericSubstrFromFront xs includeWords
    where num = numwordPrefixAsInt str
fnsff = firstNumericSubstrFromFront       -- Assign an alias for brevity

-- Convert the single line input to the corresponding calibration value
lineToCalibrationValue :: String -> Bool -> Int
lineToCalibrationValue line includeWords = (fnsff line includeWords) * 10 + (fnsff (reverse line) includeWords)

-- Extract the list of calibration values, as a list of Int,
-- from the given file input. Also consider 
extractCalibrationValues :: String -> Bool -> [Int]
extractCalibrationValues "" _ = []
extractCalibrationValues contents includeWords = [lineToCalibrationValue line includeWords | line <- lines contents]
