%%%%%%%%%%%%%%%%
% PARSING CODE %
%%%%%%%%%%%%%%%%

% Parse the input and establish establish the grid coordinates for each marking and number.
% The set of garden coordinates; includes the start's coordinates
:- dynamic(garden/1).
% The start coordinates, exactly one start coordinate exists
:- dynamic(start/1).


% Dynamically asserts facts (garden plot locations) based on the input file.
generate_facts(File) :-
    % Do file parsing
    findall(Chars, (file_line(File, Line), atom_chars(Line, Chars)), AllLines),
    foldl(interpret_line, AllLines, 0, _).


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

interpret_line(Line, LineNr, NextLineNr) :-
    NextLineNr is LineNr + 1,
    phrase(row(0, LineNr), Line).



%%%%%%%%%%%%
% DCG CODE %
%%%%%%%%%%%%

% Make strings valid inputs for the phrase/2 predicate
:- set_prolog_flag(double_quotes, chars).


% Define single characters, to explicitly draw the link with the AoC day21 problem
dot --> ".".
start --> "S".
rock --> "#".

% Each input line is a row
row(_, _) --> "".
row(X, Y) -->
    { Xnext is X + 1 },
    (
        % Assert nothing for a rock's coordinates
        rock |
        (
            % Assert 'start' for the start symbol's coordinates
            start, { assertz(start((X, Y))) } | dot
        % Assert 'garden' for start and dot symbols' coordinates
        ), { assertz(garden((X, Y))) }
    % Recursive definition
    ), row(Xnext, Y).



%%%%%%%%%%%%%
% CODE CODE %
%%%%%%%%%%%%%

% Perform depth-limited-BFS, where each update Set-ifies the updated fringe to bound the fringe size.
bfs(Fringe, 0, Fringe).
bfs(Fringe, Depth, FinalFringe) :-
    % Enforce depth-limitations; negative depth is invalid
    RecDepth is Depth - 1,
    RecDepth >= 0,
    % BFS logic
    bfs_update(Fringe, UpdatedFringe),
    set(UpdatedFringe, UniqueFringe),
    bfs(UniqueFringe, RecDepth, FinalFringe).


% Base case: The fringe has fully been expanded for one step.
% Params:
%       1) The fringe, shrinks with each recursive call,
%       2) The expanded fringe, grows with each recursive call
%       3) The function that maps a fringe element to a list of expanded elements
bfs_update([], []).
bfs_update([], _).
bfs_update([FringeHead | FringeRest], Expanded) :-
    % Recursive step, collect the extended fringe of the fringe rest first
    bfs_update(FringeRest, ExpandedRecursive),
    % Base step: Append fringe extension of fringe head to recursive results
    neighbours(FringeHead, ExpandedPartial),
    append(ExpandedPartial, ExpandedRecursive, Expanded),
    % Explicitly CUT so that only the exact solution is the output
    !.


% Define the neighbours relation: the coordinates that are at euclidean distance of
% exactly 1 from the given coordinates and are marked as 'garden', are neighbours.
% Params:
%       1) Any (X, Y) coordinate, does not have to be a garden
%       2) The list of garden coordinates at Euclidean distance 1
neighbours((X, Y), Neighbors) :-
    findall((Xn, Yn), (
        % Generator: find all gardens (coordinates) that are strictly neighbors of the given garden (coordinates)
        garden((Xn, Yn)),
        EuclideanDistance is abs(X - Xn) + abs(Y - Yn),
        EuclideanDistance == 1
    ), Neighbors).

% Define a Set datastructure; a Set is a list where each element is unique, i.e. appears exactly once.
set([], []).
% Ensure that the resulting Set is finite:
%   set([1, 1], Set), Set = [1] instead of Set = [1|_]
set([], Unique) :- append(Unique, [], Unique).
set([Elem | Rest], [Elem | UniquePartial]) :- \+ memberchk(Elem, UniquePartial), set(Rest, UniquePartial), !.
set([Elem | Rest], UniquePartial) :- memberchk(Elem, UniquePartial), set(Rest, UniquePartial), !.
