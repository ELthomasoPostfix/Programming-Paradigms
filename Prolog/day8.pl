/* The following links, in addition to the official SWISH docs, proved useful:
 * 		Text Representation (i.e. the different string literal syntaxes, atoms, ...):
 * 			https://www.swi-prolog.org/pldoc/man?section=text-representation
 * 		Recursion depth counter (i.e. counting how deep a function recurses):
 * 			https://stackoverflow.com/questions/47366029/prolog-recursion-length-count
 * 		Find all start (??a) & end (??x) nodes:
 * 			https://stackoverflow.com/a/4191431
 * 		Dynamic facts:
 * 			http://csci.viu.ca/~wesselsd/courses/csci330/code/prolog/dynamic.html#:~:text=Facts%20which%20can%20be%20altered,fact%2C%20Retracting%20an%20obsolete%20fact.
 *    DCGs:
 *      https://www.swi-prolog.org/pldoc/man?section=DCG
 *      https://www.metalevel.at/prolog/dcg
 * */



%%%%%%%%%%%%%%%%
% PARSING CODE %
%%%%%%%%%%%%%%%%

% Parse the input and establish establish the graph L-R relations.
:- dynamic(left/2).
:- dynamic(right/2).
:- dynamic(next_moves/2).

% Base case: map the empty list of moves to the initial L-R sequence of moves;
% wrap around if no more moves left.
% This rule is missing since it is described in the input file.
%
% Recursive case: pop the frontmost move from the remaining sequence of moves.
next_moves([_|Rest], Rest).

% Dynamically asserts facts (movement pattern, L-R relations) based on the input file.
% Returns false event if it succeeds.
generate_facts(File) :-
    file_line(File, Line),
    atom_chars(Line, Chars),
    interpret_line(Chars),
    % Requiring end_of_file ensures that ALL lines in the file
    % are automatically processed without needing user intervention.
    Line == end_of_file.


file_line(File, Line) :-
    setup_call_cleanup(open(File, read, In),
        stream_line(In, Line),
        close(In)).

stream_line(In, Line) :-
    repeat,
    (   read_line_to_string(In, Line0),
        Line0 \== end_of_file
    ->  Line0 = Line
    ;   !,
        fail
    ).

interpret_line(Line) :-
    % Assert the cyclicity of the movement pattern
    phrase(move_def, Line),
    assertz(next_moves([], Line));
    % Assert the left and right child/neighbour relations of the source node
    phrase(node_def(SrcNode, LeftNode, RightNode), Line),
    assertz(left(SrcNode, LeftNode)),
    assertz(right(SrcNode, RightNode)).



%%%%%%%%%%%%
% DCG CODE %
%%%%%%%%%%%%

% Make strings valid inputs for the phrase/2 predicate
:- set_prolog_flag(double_quotes, chars).

% Require node names to be exactly three characters long, NameOut is the node's name as a string
node_name(NameOut) --> { Name = [_, _, _] }, Name, { atomic_list_concat(Name, NameAtom), downcase_atom(NameAtom, NameOut) }.
node_def(SrcNode, LeftNode, RightNode) --> node_name(SrcNode), " = (", node_name(LeftNode), ", ", node_name(RightNode), ")".

% Describes the movement pattern needed to define the recursive 'next_moves' rule
move_atom --> "L" | "R".    % A singular movement instruction
move_def --> move_atom | move_atom, move_def.   % The full movement pattern



%%%%%%%%%%%%%%%%%
% SOLUTION CODE %
%%%%%%%%%%%%%%%%%

% The terminal checkers/termination conditions for the reachable predicate.
is_terminal_1(zzz).		% part 1
is_terminal_2(Node) :- is_end_atom(Node).	% part 2

recdepths_lcm(LCM) :-
  get_start_atoms(AllStartNodes),
  findall(RecDepth,
    (
      % Generator: find the recursion depth
      member(StartNode, AllStartNodes),
      get_single_recdepth(StartNode, is_terminal_2, RecDepth)
    ),
    RecDepths
  ),
  foldl(lcm_pred, RecDepths, 1, LCM).

% Get the list of all atoms that fullfill the start atom conditional.
% Note that ALL atoms that are nodes in the graph, also ALWAYS appear in at least one
% 'left' and one 'right' fact. We leverage this property to access all nodes in the grapth
% through the 'left' fact.
% Assigns the list of atoms to the given variable: AtomsLst.
get_start_atoms(AtomsLst) :-
    findall(StartAtom, (left(StartAtom, _), is_start_atom(StartAtom)), AtomsLst).

% A predicate version of the lcm Arithmetic function, for use in foldl.
lcm_pred(Arg1, Arg2, Res) :- Res is lcm(Arg1, Arg2).

% Obtain a singular recursion depth result from a reachability predicate.
% This helper method is required, because the 'reachable' predicate should
% still be able to produce infinite results, but only the very first one is
% required for computing the gcm.
% Params:
%		1) The start node from which to determine reachability of a terminal node
%		2) The recursion depth at which a terminal node was encountered
get_single_recdepth(StartNode, TerminationCondition, RecDepth) :-
    reachable(StartNode, TerminationCondition, [], RecDepth) -> !.


% Check if there is some path from the start node, using the L-R movement pattern,
% that reaches a node conforming to the termination condition at the given recursion depth.
% Params:
% 		1) The start node of the path
% 		2) The termination condition which some node on the path must conform to
% 		3) The L-R movement pattern, should be passed as an empty list
% 		4) The recursion depth at which the terminal node is found
% Reachability is defined as follows:
%		1) Reflexivity is disregarded, A --> A is not seen as reachable
%		2) Direct connection by a L-R connection is valid: A --> B where A \= B and --> represents a left or right relation
%		3) Transitive connection via any series of L-R connections is valid: A --> B --> C where A \= B & B \= C
reachable(StartNode, TerminationCondition, [], RecDepth) :-
    next_moves([], ResetMoves),
    reachable(StartNode, TerminationCondition, ResetMoves, RecDepth).
reachable(StartNode, TerminationCondition, [Direction | RemainingDirections], RecDepth) :-
  	% Base case: a direct StartNode --> EndNode connection exists AND TerminationCondition(EndNode) holds
    child(StartNode, EndNode, Direction),
    StartNode \= EndNode,
    call(TerminationCondition, EndNode),
    RecDepth is 1;

    % Recursive case: try StartNode --> IntermediateNode -->* EndNode
    child(StartNode, IntermediateNode, Direction),
    StartNode \= IntermediateNode,
    reachable(IntermediateNode, TerminationCondition, RemainingDirections, PartialRecDepth),
    RecDepth is PartialRecDepth + 1.


% Check if the first parameter is a parent of the second parameter,
% in the sense that one of its left or right 'children' is the second param.
% Params:
%		1) The node that acts as a parent
%		2) The node that acts as a child
%		3) The value indicating whether the child is a left (76) or right (82) child
child(Parent, Child, Direction) :-
    left(Parent, Child),  Direction = 'L';
    right(Parent, Child), Direction = 'R'.

% Check if the given atom ends in specifics characters.
% For the purpose of this problem, we assume an atom length
% of 3 - triplet atoms - so that manual atom deconstruction is viable.
% Lower case atom characters are also assumed.
is_start_atom(Atom) :- atom_chars(Atom, [_, _, Tail]), Tail == a.
is_end_atom(Atom)   :- atom_chars(Atom, [_, _, Tail]), Tail == z.
