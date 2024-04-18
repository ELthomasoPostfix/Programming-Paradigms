%%%%%%%%%%%%%%%%
% PARSING CODE %
%%%%%%%%%%%%%%%%

% Parse the input and establish establish the grid coordinates for each rock and garden.
% The set of garden coordinates; includes the start's coordinates
:- dynamic(garden/1).
:- dynamic(rock/1).
% Garden dimensions, part 2 specifies the gardens repeat infinitely to all sides
:- dynamic(garden_dimensions/2).
% The start coordinates, exactly one start coordinate exists
:- dynamic(start/1).


% Check if the given number is even or odd.
even(Number) :- 0 is Number mod 2.
uneven(Number) :- 1 is Number mod 2.

% Compute the manhatten distance metric of the given two grid coordinates.
manhatten_distance((X1, Y1), (X2, Y2), Distance) :- Distance is abs(X1 - X2) + abs(Y1 - Y2).

% Check if the rock is at an (un)even distance from start and (not) in the inner diamond.
diamond_rock(Rock, RequireEvenDist, RequireInDiamond) :-
    diamond_coordinate(Rock, rock, RequireEvenDist, RequireInDiamond).

% Check if the walled garden is at an (un)even distance from start and (not) in the inner diamond.
diamond_walled_garden(Rock, RequireEvenDist, RequireInDiamond) :-
    diamond_coordinate(Rock, walled_garden, RequireEvenDist, RequireInDiamond).


% A diamond coordinate is a coordinate that is located within the inner/central diamond
% of the input pattern of the problem input. This diamond is a square with
% an equal width and height, that correspond to the width and height of the input.
% Each coordinate can either be part of that diamond, or it can not be.
% Params:
%       1) The coordinates to evaluate: is it located in the diamond?
%       2) If true, require the coordinate to have an even manhatten distance from the
%       start coordinate, else require an uneven manhatten distance
%       3) If true, require the coordinate to actually be part of the diamond, else
%       require it to be outside of the diamond instead
diamond_coordinate(Coordinate, CoordGenerator, RequireEvenDist, RequireInDiamond) :-
    % Gather details
    start(Start),
    call(CoordGenerator, Coordinate),
    garden_dimensions(Width, _),

    manhatten_distance(Start, Coordinate, ManhattenDist),
    (RequireInDiamond ->
        % The coordinate is in the diamond
        ManhattenDist =< Width / 2;
        % The coordinate is outside the diamond
        ManhattenDist > Width / 2
    ),
    (RequireEvenDist ->
        % The coordinate is an even dist from the start coordinate
        even(ManhattenDist);
        % The coordinate is an uneven dist from the start coordinate
        uneven(ManhattenDist)
    ).

% A walled garden is a garden that is neighboured by exactly four rocks, immediately next to it.
% This is the only kind of "walled garden" that appears in the given input.
walled_garden((Xg, Yg)) :-
    garden((Xg, Yg)),
    rock((Xgp, Yg)), Xgp is Xg + 1,
    rock((Xgm, Yg)), Xgm is Xg - 1,
    rock((Xg, Ygp)), Ygp is Yg + 1,
    rock((Xg, Ygm)), Ygm is Yg - 1.

% Treat all adjacent coordinates in the NESW compass directions as neighbours.
% Does not consider rocks/walls.
neighbour((X, Y), (Xn, Yn)) :-
    Xn is X - 1, Yn is Y;
    Xn is X + 1, Yn is Y;
    Xn is X    , Yn is Y - 1;
    Xn is X    , Yn is Y + 1.

% Dynamically asserts facts (garden plot locations) based on the input file.
generate_facts(File) :-
    % Forget old facts
    retractall(rock(_)),
    retractall(garden(_)),
    retractall(start(_)),
    retractall(garden_dimensions(_, _)),
    % Do file parsing
    findall(Chars, (file_line(File, Line), atom_chars(Line, Chars)), AllLines),
    foldl(interpret_line, AllLines, 0, LineCount),
    % Assert garden dimensions
    AllLines = [FirstLine | _],
    length(FirstLine, LineWidth),
    assertz(garden_dimensions(LineWidth, LineCount)).

% Parse a single line of the input; a foldl predicate
interpret_line(Line, LineNr, NextLineNr) :-
    NextLineNr is LineNr + 1,
    phrase(row(0, LineNr), Line).

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
        rock, { assertz(rock((X, Y))) } |
        (
            % Assert 'start' for the start symbol's coordinates
            start, { assertz(start((X, Y))) } | dot
        % Assert 'garden' for start and dot symbols' coordinates
        ), { assertz(garden((X, Y))) }
    % Recursive definition
    ), row(Xnext, Y).



%%%%%%%%%%%%%%%
% CODE PART 1 %
%%%%%%%%%%%%%%%

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


% Map the given fringe to the neighbours of all elements.
% Params:
%       1) The fringe, shrinks with each recursive call,
%       2) The expanded fringe, grows with each recursive call
bfs_update([], []).
% Base case: The fringe has fully been expanded for one step.
bfs_update([], _).
bfs_update([FringeHead | FringeRest], Expanded) :-
    % Base step: Append fringe extension of fringe head to recursive results
    neighbours(FringeHead, ExpandedPartial),
    append(ExpandedPartial, ExpandedRecursive, Expanded),
    % Recursive step, collect the extended fringe of the fringe rest first
    bfs_update(FringeRest, ExpandedRecursive).


% Define the neighbours relation: the coordinates that are at manhatten distance of
% exactly 1 from the given coordinates and are marked as 'garden', are neighbours.
% Params:
%       1) Any (X, Y) coordinate, does not have to be a garden
%       2) The list of garden coordinates at manhatten distance 1
neighbours((X, Y), Neighbors) :-
    findall((Xn, Yn), (
        % Generator: find all gardens (coordinates) that are strictly neighbors of the given garden (coordinates)
        neighbour((X, Y), (Xn, Yn)),
        garden((Xn, Yn))
    ), Neighbors).


%%%%%%%%%%%%%%%
% CODE PART 2 %
%%%%%%%%%%%%%%%

% Compute the number of cells that would be reachable in any diamond grid without walls
% with exactly *Steps* number of steps in any direction/order.
% The total number of cells in the diamond grid can be divided into the cells reachable in
% even and uneven steps. This results in a checkered pattern of reachable cells: # for even
% steps and o for odd steps.
% Note: The size of each subsequent band grows by +4 of the previous one.
% The sum of the cells in the even bands is 4 times the sum of all even numbers
% from 0 to Steps, and that of the uneven bands is 4 times the sum of all
% uneven numbers from 0 to Steps.
%    #                     #    
%   #o#         o         # #   
%  #o#o#       o o       # # #  
% #o#o#o#     o o o     # # # #    2 uneven bands, of sizes 4 & 12  =      0*0 + 4*1 + 0*2 + 4*3 + 0*4 + ...
%#o#oSo#o#   o o o o   # # S # #
% #o#o#o#     o o o     # # # #    3 even bands, of sizes 1, 8 & 16 = 1 + (0*0 + 0*1 + 4*2 + 0*3 + 4*4 + ...)
%  #o#o#       o o       # # #     ==> The S cell is also a band in and of itself
%   #o#         o         # #   
%    #                     #    
reachable_surface(Steps, Surface) :-
    % Sum of all even numbers from 0 to Steps:
    %   Se =    0 + 2 + 4 + ... + m    where m = n if n even, else m = n - 1
    %   Se = 2*(0 + 1 + 2 + ... + m/2)
    %   Se = 2*(0 + 1 + 2 + ... + floor(n/2))
    %   Se = 2 * floor(n/2) * (floor(n/2) + 1) / 2
    Se is 2 * floor(Steps / 2) * (floor(Steps / 2) + 1) / 2,

    % Select the surface based on the even or oddness of the Steps
    ((0 is Steps mod 2) -> (
        % Steps is even
        Surface is 4 * Se + 1
    ); (
        % Steps is odd
        % Sum of all numbers from 0 to Steps:
        %   n * (n + 1) / 2
        S is Steps * (Steps + 1) / 2,
        Surface is 4 * (S - Se)
    )).


% Compute the surface outside of the inner diamond that is reachable from the
% start coordinate in an even OR uneven nr of steps, as specified by the first param.
corners_surface(RequireEvenSurface, Surface) :-
    % The input SHOULD be a square, so this dimensions unification should never fail
    garden_dimensions(Width, Width),

    % Aggregate all rocks and walled gardens in the corners/outside the inner diamond
    aggregate_all(count, diamond_rock(_, RequireEvenSurface, false), CornerRocks),
    aggregate_all(count, diamond_walled_garden(_, RequireEvenSurface, false), CornerWGardens),

    Obstacles is CornerRocks + CornerWGardens,

    % Even vs Uneven reachability in a square works like a checker board:
    % there is one more "even" cell than "uneven" cells
    NrEvenCells   is ceil(Width * Width / 2),
    NrUnevenCells is floor(Width * Width / 2),
    (RequireEvenSurface -> TileSurface is NrEvenCells;
                           TileSurface is NrUnevenCells),

    % Compute the cells contributed by the diamond
    (RequireEvenSurface -> DiamondRadiusSteps is floor(Width / 2) - 1;
                           DiamondRadiusSteps is floor(Width / 2)),
    reachable_surface(DiamondRadiusSteps, DiamondSurface),

    % The corners are the top-left, top-right, bottom-left and bottom-right sections
    % in the input that are separated from the inner diamond by a rock-free zone.
    % The surface contributed by the corners is the same as that contributed by the
    % entire input tile minus that contributed by the center diamond.
    Surface is TileSurface - DiamondSurface - Obstacles.


% Compute the surface in the entire input tile (corners + diamond) that is reachable from the
% start coordinate in an even OR uneven nr of steps, as specified by the first param.
entire_surface(RequireEvenSurface, Surface) :-
    % The input SHOULD be a square, so this dimensions unification should never fail
    garden_dimensions(Width, Width),

    % Aggregate all rocks and walled gardens in the the entire tile
    aggregate_all(count, diamond_rock(_, RequireEvenSurface, true), DiamondRocks),
    aggregate_all(count, diamond_rock(_, RequireEvenSurface, false), CornerRocks),
    aggregate_all(count, diamond_walled_garden(_, RequireEvenSurface, true), DiamondWGardens),
    aggregate_all(count, diamond_walled_garden(_, RequireEvenSurface, false), CornerWGardens),

    Obstacles is DiamondRocks + CornerRocks + DiamondWGardens + CornerWGardens,

    % oxo   In the given 3x3 grid, an even column is one that contains one more o's than
    % xox   it does x's. The inverse is true for an uneven column. It always holds that
    % oxo   there are either there are either as many even as uneven columns or there
    %       is one more even than uneven columns. So the number of even vs uneven cells
    %       differs by at most one, given any SxS square dimension S.
    NrEvenCells   is ceil(Width * Width / 2),
    NrUnevenCells is floor(Width * Width / 2),
    (RequireEvenSurface -> TileSurface is NrEvenCells;
                           TileSurface is NrUnevenCells),

    Surface is TileSurface - Obstacles.

% Compute the surface/cells reachable from the start coordinate in the
% specified number of steps.
solution(Steps, Surface) :-
    % Compute number of reachable tiles at even AND uneven distance from the start input tile.
    % A tile is a single instance of the input, tiled into a grid of instances of the input.
    garden_dimensions(Width, Width),
    % How many instances of the input are crossed in the given nr of steps?
    StepsToTiles is Steps div Width,
    reachable_surface(StepsToTiles,     NrEvenTiles),
    reachable_surface(StepsToTiles - 1, NrUnevenTiles),
    DiamondSide is StepsToTiles + 1,

    % Make assumptions based on MY problem input; paper-and-pencil-approach:
    uneven(Steps),      % The nr of steps is of the form: (steps mod width) + (steps div width)
    even(StepsToTiles), % The final tile-band is at an even "distance"/parity

    % Compute the surface (nr of cells) contributions of each type of tile.
    % paper-and-pencil: an uneven nr of steps => in "even" tiles count the
    % uneven cells, in "uneven" tiles count the even cells
    entire_surface(true,   TileEvenSurface),
    entire_surface(false,  TileUnevenSurface),
    TiledSurface is NrEvenTiles * TileUnevenSurface + NrUnevenTiles * TileEvenSurface,

    % We counted ENTIRE tiles previously, chop off the corners of edge tiles and add
    % corners of tiles just beyond the edge tiles.
    % paper-and-pencil: StepsToTiles is even => edge tiles are even tiles, the ones
    % just beyond are uneven
    corners_surface(true,  CornerEvenSurface),
    corners_surface(false, CornerUnevenSurface),
    Surface is TiledSurface - DiamondSide * CornerUnevenSurface + (DiamondSide - 1) * CornerEvenSurface.
