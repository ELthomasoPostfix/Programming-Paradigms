import Data.Char



main = do
    contents <- readFile "AoC_question_1.txt"
    -- Compute the calibration value aggregate
    let calibVals = extractCalibrationValues contents
        result = sum calibVals
        in print result


--------------------
--     PART 2     --
--------------------


-- Check if the given Char is a numerich Char
isNumericChar :: Char -> Bool
isNumericChar c = elem c ['0'..'9']

-- Use pattern matching to implicitly check whether the input string
-- is prefixed by one of the word forms of the numeric Chars.
-- Return the matching Int if it is prefixed, else return 0 by default
wordToNum :: String -> Int
wordToNum ('o':'n':'e':_)         = 1
wordToNum ('t':'w':'o':_)         = 2
wordToNum ('t':'h':'r':'e':'e':_) = 3
wordToNum ('f':'o':'u':'r':_)     = 4
wordToNum ('f':'i':'v':'e':_)     = 5
wordToNum ('s':'i':'x':_)         = 6
wordToNum ('s':'e':'v':'e':'n':_) = 7
wordToNum ('e':'i':'g':'h':'t':_) = 8
wordToNum ('n':'i':'n':'e':_)     = 9
wordToNum _                       = 0

-- Does the exact same as wordToNum, except that it checks if the input
-- string is prefixed by the reverse of the word form of a numeric Char.
wordToReverseNum :: String -> Int
wordToReverseNum ('e':'n':'o':_)         = 1
wordToReverseNum ('o':'w':'t':_)         = 2
wordToReverseNum ('e':'e':'r':'h':'t':_) = 3
wordToReverseNum ('r':'u':'o':'f':_)     = 4
wordToReverseNum ('e':'v':'i':'f':_)     = 5
wordToReverseNum ('x':'i':'s':_)         = 6
wordToReverseNum ('n':'e':'v':'e':'s':_) = 7
wordToReverseNum ('t':'h':'g':'i':'e':_) = 8
wordToReverseNum ('e':'n':'i':'n':_)     = 9
wordToReverseNum _                       = 0

-- Retrieve the first numeric Char OR numeric Char word form, iterated from the front,
-- as an Int. If there is not such Char or word form, then return 0 by default.
firstNumericSubstrFromFront :: String -> Int
firstNumericSubstrFromFront "" = 0
firstNumericSubstrFromFront str@(head:xs) | isNumericChar head          = read (head:"") :: Int
                                          | (wordToNum str) > 0         = wordToNum str
                                          | (wordToReverseNum str) > 0  = wordToReverseNum str
                                          | otherwise                   = firstNumericSubstrFromFront xs
fnsff = firstNumericSubstrFromFront       -- Assign an alias for brevity

-- Convert the single line input to the corresponding calibration value
lineToCalibrationValue :: String -> Int
lineToCalibrationValue line = (fnsff line) * 10 + (fnsff (reverse line))

-- Extract the list of calibration values, as a list of Int,
-- from the given file input. Also consider 
extractCalibrationValues :: String -> [Int]
extractCalibrationValues "" = []
extractCalibrationValues contents = [lineToCalibrationValue line | line <- lines contents]

