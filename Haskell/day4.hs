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
                         where overlap = computeCardMatchAmnt input ""
                               value = 2 ^ (overlap - 1)


-- Compute and add the card value per card.
-- Params:
--      1) The complete file input
computeTotalValueP1 :: String -> Int
computeTotalValueP1 "" = 0
computeTotalValueP1 input = sum (map (\line -> computeCardValue (snd (extractLinePrefix line ""))) (lines input))


--------------------
--     PART 2     --
--------------------

-- Parse a String representing list of space-separated
-- numbers into an actual list of the corresponding Int values.
-- Params:
--      1) The string representing a list of space-separated Ints
parseNumList :: String -> [Int]
parseNumList str = foldl aggregateNumList [0] str

-- A foldl aggregator that maps a space seperated list of numbers to a list of the corresponding Ints.
-- Must be seeded with a [0] value. The input may not contain trailing spaces.
aggregateNumList :: [Int] -> Char -> [Int]
aggregateNumList aggregator@(partialNum : finishedNums) char
    -- A number is a linear sum of its digits: 314 = 3*10^2 + 1*10^1 + 4*10^0
    | isDigit char                    = ((partialNum*10 + read (char:"")) : finishedNums)
    -- Whitespace after number found, append new placeholder 0 for next number
    | partialNum /= 0 && char == ' '  = (0 : aggregator)
    | otherwise                       = aggregator

-- Compute the the size of the win-guess overlap for a single input line
-- that has its ':' prefix removed.
-- i.e. a line of the form "num1 .. numx | num1 .. numy".
-- Params:
--      1) The input line, stripped of the prefix up until the ':' character
--      2) All characters in the input left of the '|', as a String
computeCardMatchAmnt :: String -> String -> Int
computeCardMatchAmnt (head:xs) pipeLeft
    | head == '|' = length (intersect winNums gotNums)
    | otherwise   = computeCardMatchAmnt xs (head : pipeLeft)
    where winNums = parseNumList (reverse pipeLeft)
          gotNums = parseNumList xs

-- Strip the "Card x:" prefix off the line and return (Card ID, stripped line).
-- Params:
--      1) A line of input, which MUST have a prefix containing a ':'
--      2) A value carries through recursive calls, to keep track
--         of the current, incomplete card ID substring
extractLinePrefix :: String -> String -> (Int, String)
extractLinePrefix (head:xs) idCarry
    | head == ':'  = (read (reverse idCarry), xs)
    | isDigit head = extractLinePrefix xs (head:idCarry)
    | otherwise    = extractLinePrefix xs idCarry

-- Compute the list of copies that the given Card generates.
-- Return (Card ID, list of Card copy IDs).
-- Params:
--      1) A single, raw input line
computeCopies :: String -> (Int, [Int])
computeCopies line = (id, [id + 1, id + 2 .. id + copyAmnt])
             where (id,numStr) = extractLinePrefix line ""
                   copyAmnt = computeCardMatchAmnt numStr ""

-- Update the copy amount value for the given ID in the list of
-- yet-to-process/pending Cards.
-- BUT if the given Card ID is not yet in the pending Cards list,
-- then the tuple is appended instead. Appending is a viable strategy
-- for this application, because the Cards (Card IDs) will appear
-- exclusively in ascending order of Card ID.
-- Params:
--      1) [(Card ID, additional copy amount)], each element is used for updating in order,
--         or they are appended in order if they are not in the pending Cards list yet
--      2) [(Card ID, copy amount)]
updateCopyAmnts :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
updateCopyAmnts [] pendingCards = pendingCards  -- Tuples to update with
updateCopyAmnts tuples [] = tuples              -- Target list empty, append all tuples in order
updateCopyAmnts tuples@(thead:txs) (pchead:pcxs)
    | updateCardId == cardID = (cardID, copyAmnt + addAmnt) : (updateCopyAmnts txs pcxs)  -- Update using tuple head, do recursive for remaining tuples
    | otherwise              = pchead : (updateCopyAmnts tuples pcxs)  -- Retain un-updated pending Card
    where (cardID, copyAmnt) = pchead
          tuple@(updateCardId, addAmnt) = thead

-- Compute the total amount of Cards won.
-- The following approach is used: maintain a list of copy aggregate values per Card ID.
-- i.e. each time a Card generates copies, we keep track of how many copies of that Card
-- have already been generated to date, in a list, per Card ID.
-- Since the Cards appear in ascending order by ID in the input, we know for certain that
-- if the aggregate list is also sorted in ascending order by Card ID, then we can
-- apply a two-pointer-approach. One "pointer" is the current Card input line. The second
-- pointer is the head of aggregate list.
-- Due to the ascending order, a given Card ID will only become the head "pointer" when
-- the input line "pointer" also points to the same Card ID. This means that, every time
-- that the two "pointers" point to the same Card ID, that tuple may be dropped from the
-- aggregate list. This implies that the aggregate list's length is BOUNDED by the largest
-- winning-chosen number intersection (i.e. the largest amount of duplicates that any one
-- Card generates).
--
-- Params:
--      1) A list of all individual lines, ordered by Card ID in ascending order (low to high)
--      2) A list of (Card ID, copy amount) tuples, which functions as the list of pending
--         Card's amount of copies generated to date
computeTotalValueP2 :: [String] -> [(Int, Int)] -> Int
computeTotalValueP2 [] copyAggregates = 0 -- Cards will never make you copy a card past the end of the table.
computeTotalValueP2 (line:lxs) copyAggregates
    | copiesIDs == []              = thisCopyAmnt + (computeTotalValueP2 lxs selectedAggregates)
    | otherwise                    = thisCopyAmnt + (computeTotalValueP2 lxs (updateCopyAmnts copyAmnts selectedAggregates))
    where (id,copiesIDs) = computeCopies line
          (cahead:caxs) = if copyAggregates /= []
                          then copyAggregates
                          else [(-1, 1)]        -- Dummy value to catch the case where the aggregates list is empty
          (caid,caCopyAmnt) = cahead
          thisCopyAmnt = if id == caid
                         then 1 + caCopyAmnt    -- Other Cards DID generate copies of this Card ID; one original card + the copy amount = total amount received of the given Card ID
                         else 1                 -- Other Cards did NOT generate a copy for this Card ID; one original card = total amount received of the given Card ID
          copyAmnts = map (\cid -> (cid, thisCopyAmnt)::(Int,Int)) copiesIDs
          selectedAggregates = if id == caid        -- The list of aggregates to update and use for recursion
                               then caxs            -- The current Card id relates to a tuple in the aggregates, pop that tuple
                               else copyAggregates  -- The current Card id does NOT relate to a tuple in the aggregates, no action needed
