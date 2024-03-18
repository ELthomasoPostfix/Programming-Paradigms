


main = do
    contents <- readFile "day3.txt"
    -- Part 1: Compute nr of ways in which to beat the records
    let solutions = computeIndivSolutions (lines contents)
        result = product solutions
        in putStrLn ("part 1: " ++ (show result) ++ "  " ++ (show solutions))
    -- Part 2: Compute ...

    return ()



-------------------------
--     PARTS 1 & 2     --
-------------------------

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

-- Compute how many seconds the button has to be pressed at least and at most.
-- To do this, compute the solution to the following quadratic equation:
--      distance = (raceTime - pressTime) * pressTime
--   =>        0 = -distance + raceTime * pressTime - pressTime^2
-- where we solve for pressTime. The output is a pair of solutions, where
-- the first element is the smaller one of the two.
-- Pre-conditions:
--      1) A solution for distance MUST exist
--      2) distance may NOT be the global max of possible distances
-- Params:
--      1) distance, the distance to solve for
--      2) raceTime, the total race time to solve for
solveQuadEq :: Int -> Int -> (Int, Int)
solveQuadEq distance raceTime =
    (
        (floor solFloor)  + 1,
        min ((ceiling solCeil) - 1) raceTime
    )
    where r    = fromIntegral raceTime
          disc = (r^2 - fromIntegral (4 * distance))
          div  = -2.0
          solFloor = ((-r) + sqrt disc) / div
          solCeil  = ((-r) - sqrt disc) / div

computeIndivSolutions :: [String] -> [Int]
computeIndivSolutions (timeLine:distLine:[]) =
    -- error (show indivSols)
    -- foldl (\aggr (solFloor, solCeil) -> aggr * (solCeil - solFloor + 1)) 1 indivSols
    map (\(solFloor, solCeil) -> solCeil - solFloor + 1) indivSols
    where times = parseNumList (snd (splitOnce ':' timeLine))
          dists = parseNumList (snd (splitOnce ':' distLine))
          indivSols = zipWith solveQuadEq dists times
