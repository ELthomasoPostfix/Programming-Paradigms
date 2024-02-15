import Data.Char



main = do
    contents <- readFile "AoC_question_1.txt"
    -- Compute the calibration value aggregate
    let calibVals = extractCalibrationValues contents
        result = sum calibVals
        in print result


--------------------
--     PART 1     --
--------------------


-- Check if the given Char is a numerich Char
isNumericChar :: Char -> Bool
isNumericChar c = elem c ['0'..'9']

-- Retrieve the first numeric Char, iterated from the front.
-- If there is not such Char, then return '0' by default.
firstNumericCharFromFront :: String -> Char
firstNumericCharFromFront "" = '0'
firstNumericCharFromFront (head:xs) | isNumericChar head = head
                                    | otherwise          = firstNumericCharFromFront xs
fncff = firstNumericCharFromFront       -- Assign an alias for brevity

-- Convert the single line input to the corresponding calibration value
lineToCalibrationValue :: String -> Int
lineToCalibrationValue line = read ((fncff line) : (fncff (reverse line)) : "") :: Int

-- Extract the list of calibration values, as a list of Int,
-- from the given file input.
extractCalibrationValues :: String -> [Int]
extractCalibrationValues "" = []
extractCalibrationValues contents = [lineToCalibrationValue line | line <- lines contents]
