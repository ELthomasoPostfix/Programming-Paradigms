import Data.Char
import Numeric

main = do
    contents <- readFile "day18.txt"
    let surface = trapezoidFormula contents parseMethodP1
        in putStrLn ("part 1: " ++ show surface)
    let surface = trapezoidFormula contents parseMethodP2
        in putStrLn ("part 2: " ++ show surface)
    return ()


-------------------------
--     PARTS 1 & 2     --
-------------------------

type Vertex = (Int, Int)
-- An input line parsing method should extract the direction and distance
-- to walk from one vertex to the next during the excavation.
type LineParseMethod = (String -> (Char, Int))

-- The line parsing method for part 1
parseMethodP1 :: LineParseMethod
parseMethodP1 line =
    (dir, read dist)
    where ((dir:""):dist:_:[]) = words line

-- The line parsing method for part 2
parseMethodP2 :: LineParseMethod
parseMethodP2 line =
    (numToDirection (last hexstr), fst (readHex (take 5 hexstr) !! 0))
    where (_:_:(_:_:hexcol):[]) = words line
          hexstr = init hexcol

-- Convert a number in the range [0..3] to the corresponding direction Char.
numToDirection :: Char -> Char
numToDirection numChar = ['R', 'D', 'L', 'U'] !! read (numChar:"")

-- Convert a direction Char to the corresponding directional vector.
directionToVec :: Char ->  (Int, Int)
directionToVec dir
    | dir == 'U' = (0, 1)
    | dir == 'R' = (1, 0)
    | dir == 'D' = (0, -1)
    | dir == 'L' = (-1, 0)
    | otherwise  = error ("Unknown direction: got " ++ (dir:"") ++ " expected one of: 'U', 'R', 'D', 'L'")

-- See https://stackoverflow.com/a/18158515
-- Compute the surface of the irregular geometry shape using the trapezoid formula.
-- Params:
--      1) The full input file contents
--      2) The input line parsing method
trapezoidFormula :: String -> LineParseMethod -> Int
trapezoidFormula contents parseMethod =
    -- Do +1 to compensate for the initial 1x1 are dug out BEFORE
    -- the walk of the excavator around the perimeter
    round (1.0 + partialSurface / 2.0)
    where walkAggregate  = foldr (trapezoidPerimeterWalk parseMethod) ((0,0), 0) (lines contents)
          partialSurface = fromIntegral (snd (walkAggregate))

-- Perform the entire perimeter walk of the excavator by iteratively updating
-- the current vertex coordinate and surface area aggregate.
-- This method is intended as a foldr aggregator method.
-- Params:
--      1) The line parsing method; curry this param before use as an aggregator
--      2) An input line; an input element to foldr over
--      3) The running surface area aggregate
trapezoidPerimeterWalk :: LineParseMethod -> String -> (Vertex, Int) -> (Vertex, Int)
trapezoidPerimeterWalk parseMethod line (vertex@(x, y), surface) =
    (nextVertex, surface + trapezoidSurface + trenchEdgeLength)
    where nextVertex@(x', y') = computeNextVertex vertex line parseMethod
          -- The partial formula over which the trapezoid formula sums
          trapezoidSurface = (y + y') * (x - x')
          -- The length of the edge that is dug out during the initial walk
          trenchEdgeLength = abs (y' - y) + abs (x' - x)

-- Compute the next vertex of the irregular geometry shape based on the current vertex and an input line.
-- Params:
--      1) The current vertex (x, y) cartesian coordinates
--      2) The input line
-- Return: ((x', y'), hex color string, e.g. "FFFFFF")
computeNextVertex :: Vertex -> String -> LineParseMethod -> Vertex
computeNextVertex vertex@(x,y) line parseMethod =
    (x + distance * dirx, y + distance * diry)
    where (dir, distance) = parseMethod line
          (dirx, diry)    = directionToVec dir
