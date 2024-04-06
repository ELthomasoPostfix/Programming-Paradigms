%%%%%%%%%%%%%%%%
% PARSING CODE %
%%%%%%%%%%%%%%%%

% Parse the input and establish establish the grid coordinates for each marking and number.
% The set of garden coordinates; includes the start's coordinates
:- dynamic(garden/1).
% Garden dimensions, part 2 specifies the gardens repeat infinitely to all sides
:- dynamic(garden_dimensions/2).
% The start coordinates, exactly one start coordinate exists
:- dynamic(start/1).


% Assert garden cyclicity/infinity, a coordinate is a garden if
% the corresponding coordinate in the input grid is a garden.
garden_inf((X, Y)) :-
    % The coordinate lies in the original grid dimensions
    garden((X, Y));
    % The coordinate lies in some multiple of the grid dimensions
    garden_dimensions(Width, Length),
    Xmod is X mod Width,
    Ymod is Y mod Length,
    garden((Xmod, Ymod)).

% Treat all adjacent coordinates in the NESW compass directions as neighbours.
neighbour((X, Y), (Xn, Yn)) :-
    Xn is X - 1, Yn is Y;
    Xn is X + 1, Yn is Y;
    Xn is X    , Yn is Y - 1;
    Xn is X    , Yn is Y + 1.


% Dynamically asserts facts (garden plot locations) based on the input file.
generate_facts(File) :-
    % Do file parsing
    findall(Chars, (file_line(File, Line), atom_chars(Line, Chars)), AllLines),
    foldl(interpret_line, AllLines, 0, LineCount),
    % Assert garden dimensions
    AllLines = [FirstLine | _],
    length(FirstLine, LineWidth),
    assertz(garden_dimensions(LineWidth, LineCount)).


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
    list_to_set(UpdatedFringe, UniqueFringe),
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
        neighbour((X, Y), (Xn, Yn)),
        garden_inf_single((Xn, Yn))
    ), Neighbors).


% Call `garden_inf` with a CUT, to ensure only a single garden's coords are returend.
% When a coordinate falls in the original dimensions, both OR clauses evaluate to true
% so that two results would be returned. This bloats the fringe unnecessarily.
garden_inf_single(Garden) :- garden_inf(Garden), !.
