import Data.Char
import Data.List



main = do
    contents <- readFile "day5.txt"
    print (computeMinLocation contents)
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


-- Transform the intermediates list of soon to be locations into two distinct lists:
--      1) The sub-list of values that did NOT fit the given range, and so remain untransformed
--      2) The sub-list of transformed values, according to the range mapping
-- Params:
--      1) The range to transform with, of the form (destination range start, source range start, range length)
--      2) The list of values to transform
transformByRange :: (Int, Int, Int) -> [Int] -> ([Int], [Int])
transformByRange range@(rangeStartDest, rangeStartSrc, rangelen) soonToBeLocations =
    (untransformed, transformed)
    where toTransform   = [ihead | ihead <- soonToBeLocations,
                                   rangeStartSrc <= ihead && ihead <= (rangeStartSrc + rangelen - 1)]
          transformed   = [rangeStartDest + ihead - rangeStartSrc | ihead <- toTransform]
          untransformed = soonToBeLocations \\ toTransform

-- Params:
--      1) The list of input lines
--      2) The list of intermediates that have NOT yet been transformed using a range mapping
--      3) The list of intermediates that have that HAVE been transformed using a range mapping
computeLocations :: [String] -> [Int] -> [Int] -> [Int]
computeLocations [] untransformedElems transformedElems = untransformedElems ++ transformedElems
computeLocations (('s':'e':'e':'d':'s':':':seedLst):ignore:lxs) _ _ = computeLocations lxs (parseNumList seedLst "") []  -- This SHOULD be the entry point; every input file starts with a seed line
computeLocations (lhead:lxs) untransformedElems transformedElems
    | isMapEnd    = computeLocations lxs (untransformedElems ++ transformedElems) []   -- The untransformed elements did not fit any range, map them one to one to a transformed value
    | isRangeLine = computeLocations lxs remainingUntransformed (addedTransformed ++ transformedElems)     -- Use range mapping to transform some elements; other range input lines may still follow this one
    | otherwise   = computeLocations lxs untransformedElems transformedElems        -- Ignore other input lines
    where isMapEnd    = lhead == ""
          isRangeLine = isDigit (head lhead)    -- Should only be used if the line is NOT the empty line
          (rsdest:rssrc:rangelen:[]) = parseNumList lhead "" -- Each range line consists of EXACTLY 3 numbers (destination range start, source range start, range length) = (rsdest, rangeStartSrc, rangelen)
          (remainingUntransformed, addedTransformed) = transformByRange (rsdest, rssrc, rangelen) untransformedElems

-- Compute the smallest seed location given the input file.
-- Params:
--      1) The input file as a String
computeMinLocation :: String -> Int
computeMinLocation input = minimum (computeLocations (lines input) [] [])