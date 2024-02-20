import Data.List
import Data.Char



main = do
    contents <- readFile "day4.txt"
    print (computeTotalValue (lines contents) [])
    return ()


--------------------
--     PART 2     --
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

-- Compute the the size of the win-guess overlap for a single input line
-- that has its ':' prefix removed.
-- i.e. a line of the form "num1 .. numx | num1 .. numy".
-- Params:
--      1) The input line, stripped of the prefix up until the ':' character
--      2) All characters in the input left of the '|', as a String
--      3) All characters in the input right of the '|', as a String
--      4) A Bool that represents whether the '|' has been seen already in the input
computeCardMatchAmnt :: String -> String -> String -> Bool -> Int
computeCardMatchAmnt "" pipeLeft pipeRight _ = length (intersect winNums gotNums)
                                    where winNums = parseNumList pipeLeft ""
                                          gotNums = parseNumList pipeRight ""
computeCardMatchAmnt (head:xs) pipeLeft pipeRight pipeSeen
    | head == '|' = computeCardMatchAmnt xs pipeLeft pipeRight True
    | pipeSeen    = computeCardMatchAmnt xs pipeLeft (pipeRight ++ (head:"")) pipeSeen
    | otherwise   = computeCardMatchAmnt xs (pipeLeft ++ (head:"")) pipeRight pipeSeen

-- Strip the "Card x:" prefix off the line and return (Card ID, stripped line).
-- Params:
--      1) A line of input, which MUST have a prefix containing a ':'
--      2) A value carries through recursive calls, to keep track
--         of the current, incomplete card ID substring
extractLinePrefix :: String -> String -> (Int, String)
extractLinePrefix (head:xs) idCarry
    | head == ':'  = (read idCarry, xs)
    | isDigit head = extractLinePrefix xs (idCarry ++ (head:""))
    | otherwise    = extractLinePrefix xs idCarry

-- Compute the list of copies that the given Card generates.
-- Return (Card ID, list of Card copy IDs).
-- Params:
--      1) A single, raw input line
computeCopies :: String -> (Int, [Int])
computeCopies line = (id, [id + 1, id + 2 .. id + copyAmnt])
             where (id,numStr) = extractLinePrefix line ""
                   copyAmnt = computeCardMatchAmnt numStr "" "" False

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
computeTotalValue :: [String] -> [(Int, Int)] -> Int
computeTotalValue [] copyAggregates = 0 -- Cards will never make you copy a card past the end of the table.
computeTotalValue (line:lxs) copyAggregates
    | copiesIDs == []              = thisCopyAmnt + (computeTotalValue lxs selectedAggregates)
    | otherwise                    = thisCopyAmnt + (computeTotalValue lxs (updateCopyAmnts copyAmnts selectedAggregates))
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
