import Data.Char
import Data.List



main = do
    contents <- readFile "day5.txt"
    putStrLn ("part 1: " ++ show (computeMinLocation contents seedsAsSingles))
    putStrLn ("part 2: " ++ show (computeMinLocation contents seedsAsDoubles))
    return ()


-------------------------
--     PARTS 1 & 2     --
-------------------------

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


-- Transform a list of elements into a list of pairs.
-- The input list is REQUIRED to be of event length.
pairify :: [x] -> [(x, x)]
pairify [] = []
pairify lst@(head1:head2:lxs) = (head1, head2) : (pairify lxs)
pairify _ = error "Cannot pairify a list of uneven length"


-- Split the splitRange param into a pair of lists:
--      1) A list containing at most one subrange that DOES overlap with the referenceRange
--      2) A list containing zero or more subranges that DON'T overlap with the referenceRange.
-- There are multiple cases to consider, the comments will represent the split range
-- using brackets, [], and the reference range using braces, {}. e.g. when the two
-- ranges overlap partially, we may represent this as follows: { [ } ] => overlap = [ }
-- Note that we are only determining overlap of the split range with the reference range.
-- Any subrange is subsequently guaranteed to be completely included inside the split range.
-- Consider the following figure, which depicts all cases, and where overlap is represented
-- using x's:
--         {xxxxxx}  
--     [   xxx][xxx   ]
--     [   xxxxxxxx   ]
-- [  ]               [  ]
-- Params:
--      1) The range to split into subranges
--      2) The range to determine overlap with
splitRangeByOverlap :: (Int, Int) -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
splitRangeByOverlap splitRange@(splitMin, splitLen) referenceRange@(refMin, refLen)
    -- case: [ ] { } = No overlap, the split range is located entirely left of the reference range
    | splitMax < refMin = ([], [splitRange])
    -- case: { } [ ] = No overlap, the split range is located entirely right of the reference range
    | refMax < splitMin = ([], [splitRange])
    | (refMin <= splitMax) && (splitMax <= refMax) =
        if splitMin < refMin
        -- case: [ { ] } = partial overlap, right side of split range overlaps, left side does not
        then ([(refMin, splitMax - refMin + 1)], [leftNonOverlap])
        -- case: { [ ] } = complete overlap, enveloped by ref, entire split range overlaps
        else ([splitRange], [])
    -- case: { [ } ] = partial overlap, left side of split range overlaps, right side does not; envelopment case is already checked previously
    | (refMin <= splitMin) && (splitMin <= refMax) =
        ([(splitMin, refMax - splitMin + 1)], [rightNonOverlap])
    -- case: [ { } ] = partial overlap, ref enveloped by split, three subranges are generated: [ { AND { } AND } ], where only { } is seen as overlap
    | otherwise = ([referenceRange], [leftNonOverlap, rightNonOverlap])
    where splitMax = splitMin + splitLen - 1
          refMax   = refMin + refLen - 1
          leftNonOverlap = (splitMin, refMin - splitMin)
          rightNonOverlap = (refMax + 1, splitMax - refMax)


-- Transform the intermediate seed ranges list into two distinct lists:
--      1) The sub-list of values that did NOT fit the given range, and so remain untransformed
--      2) The sub-list of transformed values, according to the range mapping
-- Params:
--      1) The range to transform with, of the form (destination range start, source range start, range length)
--      2) The list of values to transform
transformByRange :: (Int, Int, Int) -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
transformByRange _ [] = ([], [])
transformByRange range@(rangeStartDest, rangeStartSrc, rangelen) (lrange@(lrangeMin, lrangeLen):lrxs) =
    (nonOverlappingSubRanges ++ untransformedElems, transformedRanges ++ transformedElems)
    where (toTransform, nonOverlappingSubRanges) = splitRangeByOverlap lrange (rangeStartSrc, rangelen)
          transformedRanges = [ (rangeStartDest + relRanMin - rangeStartSrc, relRanLen) | (relRanMin, relRanLen) <- toTransform ]
          (untransformedElems, transformedElems) = transformByRange range lrxs


-- A seed range is a tuple = (seed range start, seed range length).
-- Params:
--      1) The list of input lines
--      2) The list of intermediate seed ranges that have NOT yet been transformed using a range mapping
--      3) The list of intermediate seed ranges that have that HAVE been transformed using a range mapping
computeLocations :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
computeLocations [] untransformedElems transformedElems = untransformedElems ++ transformedElems
computeLocations (lhead:lxs) untransformedElems transformedElems
    -- The untransformed elements did not fit any range, map them one to one to a transformed value
    | isMapEnd    = computeLocations lxs (untransformedElems ++ transformedElems) []
    -- Use range mapping to transform some elements; other range input lines may still follow this one
    | isRangeLine = computeLocations lxs remainingUntransformed (addedTransformed ++ transformedElems)
    -- Ignore other input lines
    | otherwise   = computeLocations lxs untransformedElems transformedElems
    where isMapEnd    = lhead == ""
          -- Should only be used if the line is NOT the empty line
          isRangeLine = isDigit (head lhead)
          -- Each range line consists of EXACTLY 3 numbers (destination range start, source range start, range length) = (rsdest, rangeStartSrc, rangelen)
          (rsdest:rssrc:rangelen:[]) = parseNumList lhead ""
          (remainingUntransformed, addedTransformed) = transformByRange (rsdest, rssrc, rangelen) untransformedElems

-- Parse the very first input line, containing the seeds numbers, into a list
-- of ranges suitable for part 1 of AoC day 5. Each seed will be mapped to
-- a range of length 0, meaning the range contains only the seed itself; (seed, 0).
-- Params:
--      1) The first line of the input
seedsAsSingles :: String -> [(Int, Int)]
seedsAsSingles ('s':'e':'e':'d':'s':':':seedLst) = map (\seed -> (seed, 0)) (parseNumList seedLst "")

-- Parse the very first input line, containing the seeds numbers, into a list
-- of ranges suitable for part 2 of AoC day 5. Each pair of seeds will be mapped to
-- a range, where the first seed is the range start and the second seed the range
-- length; (seed1, seed2) = (rangeStart, rangeLen).
-- Params:
--      1) The first line of the input
seedsAsDoubles :: String -> [(Int, Int)]
seedsAsDoubles ('s':'e':'e':'d':'s':':':seedLst) = pairify (parseNumList seedLst "")


-- Compute the smallest seed location given the input file.
-- This SHOULD be the entry point; every input file starts with a seed line
-- Params:
--      1) The input file as a String
--      2) The method to parse the seeds line into the appropriate seed ranges
computeMinLocation :: String -> (String -> [(Int, Int)]) -> Int
computeMinLocation input seedParser = minimum (map (\(seedRangeMin, _) -> seedRangeMin) (computeLocations mapInputLines initialRanges []))
                                      where (seedsLine:_:mapInputLines) = lines input
                                            initialRanges = seedParser seedsLine
