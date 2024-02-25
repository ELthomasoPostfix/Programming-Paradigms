/* The following links, in addition to the official SWISH docs, proved useful:
 * 		Text Representation (i.e. the different string literal syntaxes, atoms, ...):
 * 			https://www.swi-prolog.org/pldoc/man?section=text-representation
 * 		Recursion depth counter (i.e. counting how deep a function recurses):
 * 			https://stackoverflow.com/questions/47366029/prolog-recursion-length-count
 * 		Find all start (??a) & end (??x) nodes:
 * 			https://stackoverflow.com/a/4191431
 * */

% Check if the given atom ends in specifics characters.
% For the purpose of this problem, we assume an atom length
% of 3 - triplet atoms - so that manual atom deconstruction is viable.
% Lower case atom characters are also assumed.
is_start_atom(Atom) :- atom_chars(Atom, [_, _, Tail]), Tail == a.
is_end_atom(Atom)   :- atom_chars(Atom, [_, _, Tail]), Tail == z.

% Get the list of all atoms that fullfill the chosen start atom or end atom conditional.
% Note that ALL atoms that are nodes in the graph, also ALWAYS appear in at least one
% 'left' and one 'right' fact. We leverage this property to access all nodes in the grapth
% through the 'left' fact.
% Assigns the list of atoms to the given variable: AtomsLst.
get_start_atoms(AtomsLst) :-
    findall(StartAtom, (left(StartAtom, _), is_start_atom(StartAtom)), AtomsLst).
get_end_atoms(AtomsLst) :-
    findall(EndAtom, (left(EndAtom, _), is_end_atom(EndAtom)), AtomsLst).


% Check if the given character code corresponds to a left or right move.
% Note: the following character codes hold: 'L' == 76, 'R' == 82
is_left_move(CharCode)  :- char_code('L', LCode), CharCode == LCode.
is_right_move(CharCode) :- char_code('R', RCode), CharCode == RCode.


/* Map the given list of current nodes according to the specified left/right
 * rule, to produce a list of advanced nodes.
 * Params:
 * 		1) The sourc list of nodes to advance/map
 * 		2) The rule to advance with; MUST be one of the following atoms: 'left', 'right'
 * 		3) The destination list for advanced/mapped nodes
 */
advance_nodes(CurrentNodes, LRRule, AdvancedNodes) :-
    findall(NextNode,
            (
            	member(CurrNode, CurrentNodes),
                call(LRRule, CurrNode, NextNode)
            ),
            AdvancedNodes).



% Base case: ??z is the terminal node, and ??z can be reached from ??z in zero steps.
follow_path_multi(CurrentNodes, _, 0) :-
    forall(member(CurrNode, CurrentNodes), is_end_atom(CurrNode)).
% Recursive cases: find a path from the list of current nodes to some list of terminal nodes,
% by navigating the left and right nodes of the current nodes. This navigation MUST follow the
% L-R movement pattern.
% Prevent infinite recursion: if EVERY left(CurrNode) or right(CurrNode) is a self loop, then backtrack.
% Provide the path length as output through the third parameter. The following SO post served as inspiration: https://stackoverflow.com/questions/47366029/prolog-recursion-length-count
follow_path_multi(CurrentNodes, [], PathLength) :-
    next_moves([], ResetMoves),
    follow_path_multi(CurrentNodes, ResetMoves, PartialPathLength),
    PathLength is PartialPathLength.
follow_path_multi(CurrentNodes, [CurrMove|RemainingMoves], PathLength) :-
    is_left_move(CurrMove),
    \+ forall(member(CurrNode, CurrentNodes), left(CurrNode, CurrNode)),
    advance_nodes(CurrentNodes, left, LeftAdvancedNodes),
    follow_path_multi(LeftAdvancedNodes, RemainingMoves, PartialPathLength),
    PathLength is PartialPathLength + 1.
follow_path_multi(CurrentNodes, [CurrMove|RemainingMoves], PathLength) :-
    is_right_move(CurrMove),
    \+ forall(member(CurrNode, CurrentNodes), right(CurrNode, CurrNode)),
    advance_nodes(CurrentNodes, right, RightAdvancedNodes),
    follow_path_multi(RightAdvancedNodes, RemainingMoves, PartialPathLength),
    PathLength is PartialPathLength + 1.

