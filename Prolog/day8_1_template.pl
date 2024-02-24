/* The following links, in addition to the official SWISH docs, proved useful:
 * 		Text Representation (i.e. the different string literal syntaxes, atoms, ...):
 * 			https://www.swi-prolog.org/pldoc/man?section=text-representation
 * 		Recursion depth counter (i.e. counting how deep a function recurses):
 * 			https://stackoverflow.com/questions/47366029/prolog-recursion-length-count
 * */



% Check if the given character code corresponds to a left or right move.
% Note: the following character codes hold: 'L' == 76, 'R' == 82
is_left_move(CharCode)  :- char_code('L', LCode), CharCode == LCode.
is_right_move(CharCode) :- char_code('R', RCode), CharCode == RCode.


% Base case: zzz is the terminal node, and zzz can be reached from zzz in zero steps.
follow_path(zzz, _, 0).
% Recursive cases: find a path from the current node to the terminal node, by navigating the left and right nodes of the current node.
% Prevent infinite recursion: if left(CurrNode) or right(CurrNode) is a self loop, then backtrack.
% Provide the path length as output through the third parameter. The following SO post served as inspiration: https://stackoverflow.com/questions/47366029/prolog-recursion-length-count
follow_path(CurrNode, [], PathLength) :-
    next_moves([], ResetMoves), follow_path(CurrNode, ResetMoves, PartialPathLength),
    PathLength is PartialPathLength.
follow_path(CurrNode, [CurrMove|RemainingMoves], PathLength) :-
    is_left_move(CurrMove),
    left(CurrNode, LeftNode),
    CurrNode \= LeftNode,
    follow_path(LeftNode, RemainingMoves, PartialPathLength),
    PathLength is PartialPathLength + 1.
follow_path(CurrNode, [CurrMove|RemainingMoves], PathLength) :-
    is_right_move(CurrMove),
    right(CurrNode, RightNode),
    CurrNode \= RightNode,
    follow_path(RightNode, RemainingMoves, PartialPathLength),
    PathLength is PartialPathLength + 1.

