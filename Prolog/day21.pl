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

% An enum mapping from corner/quadrant atoms to the operators needed
% to categorize any coordinate in the corresponding quadrant, in relation
% to the start coordinate.
% i.e. (X, Y) is top_right of start coordinate (Sx, Sy) iff. X > Sx & Y > Sy
quadrant(top_left,     (<, >)).
quadrant(top_right,    (>, >)).
quadrant(bottom_left,  (<, <)).
quadrant(bottom_right, (>, <)).

% Check if the given coordinate is in the specified quadrant in
% relation to the start coordinate.
is_in_quadrant((X, Y), Quadrant) :-
    start((Sx, Sy)),
    quadrant(Quadrant, (OperatorX, OperatorY)),
    % Check quadrant restrictions
    call(OperatorX, X, Sx), call(OperatorY, Y, Sy).

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
%       require it to be outside of the diamond
diamond_coordinate(Coordinate, CoordGenerator, RequireEvenDist, RequireInDiamond) :-
    % Gather details
    start(Start),
    call(CoordGenerator, Coordinate),
    garden_dimensions(Width, _),

    % Check if coordinate lies in the inner diamond of the input pattern
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
        rock, { assertz(rock((X, Y))) } |
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


% Define the neighbours relation: the coordinates that are at manhatten distance of
% exactly 1 from the given coordinates and are marked as 'garden', are neighbours.
% Params:
%       1) Any (X, Y) coordinate, does not have to be a garden
%       2) The list of garden coordinates at manhatten distance 1
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




%%%%%%%%%%
% PART 2 %
%%%%%%%%%%

% Compute the number of cells that would be reachable in any diamond grid without walls
% with exactly *Steps* number of steps in any direction/order.
% The total number of cells in the diamond grid can be divided into the cells reachable in
% even and uneven steps. This results in a checkered pattern of reachable cells: # for even
% steps and o for odd steps.
% Note: The size of the "even bands" grows by +8 each band, and that of the "uneven bands"
% grows +8 each band. The sum of the cells in the even bands is 4 times the sum of all
% even numbers from 0 to Steps, and that of the uneven bands is 4 times the sum of all
% uneven numbers from 0 to Steps.
%    #                     #    
%   #o#         o         # #   
%  #o#o#       o o       # # #  
% #o#o#o#     o o o     # # # #    2 uneven bands, sizes 4 & 12  =      0*0 + 4*1 + 0*2 + 4*3 + 0*4 + ...
%#o#oSo#o#   o o o o   # # S # #
% #o#o#o#     o o o     # # # #    3 even bands, sizes 1, 8 & 16 = 1 + (0*0 + 0*1 + 4*2 + 0*3 + 4*4 + ...)
%  #o#o#       o o       # # #     ==> The S cell is also a band in and of itself
%   #o#         o         # #   
%    #                     #    
reachable_surface(Steps, Surface) :-
    % Sum of all numbers from 0 to Steps:
    %   n * (n + 1) / 2
    S is Steps * (Steps + 1) / 2,
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
        Surface is 4 * (S - Se)
    )).


solution(Steps, Surface) :-
    % Compute how many rock and walled garden statistics in/out of the inner diamond
    aggregate_all(count, diamond_rock(_, true, true), EvenRocksInOneDiamond),
    aggregate_all(count, diamond_walled_garden(_, true, true), EvenWGardensInOneDiamond),
    aggregate_all(count, diamond_rock(_, false, false), UnevenRocksOutsideOneDiamond),
    aggregate_all(count, diamond_walled_garden(_, false, false), UnevenWGardensOutsideOneDiamond),
    % Compute number of reachable cells
    reachable_surface(Steps, RocklessSurface),
    % The area of the diamond-shaped tile grid contains an SxS square of inner diamonds
    garden_dimensions(Width, _),
    SquareSide is (Steps div Width) * 2 + 1,
    Sf is floor(SquareSide / 2),
    Sc is ceil(SquareSide / 2),
    DiamondCountEven   is Sc * Sc + Sf * Sf,
    DiamondCountUneven is Sf * Sc + Sc * Sf,
    TotalObstacles is
        (EvenRocksInOneDiamond + EvenWGardensInOneDiamond) * DiamondCountEven +
        (UnevenRocksOutsideOneDiamond + UnevenWGardensOutsideOneDiamond) * DiamondCountUneven,
    Surface is RocklessSurface - TotalObstacles,


    % ALL THE DIAMONDS AT AN EVEN DIST/PARITY ARE TRUE INNER DIAMONDS,
    % THE OTHER DIAMONDS ARE MADE UP OF OUTER QUADRANTS OF ADJACENT SQUARES

    write(EvenRocksInOneDiamond),
    write("\n"),
    write(EvenWGardensInOneDiamond),
    write("\n"),
    write(UnevenRocksOutsideOneDiamond),
    write("\n"),
    write(UnevenWGardensOutsideOneDiamond),
    write("\n")
    .

solution4(Steps, Surface) :-
    % Compute how many rock and walled garden statistics in/out of the inner diamond
    aggregate_all(count, diamond_rock(_, true, true), EvenRocksInOneDiamond),
    aggregate_all(count, diamond_rock(_, false, true), UnevenRocksInOneDiamond),
    aggregate_all(count, diamond_rock(_, true, false), EvenRocksOutsideOneDiamond),
    aggregate_all(count, diamond_rock(_, false, false), UnevenRocksOutsideOneDiamond),
    aggregate_all(count, diamond_walled_garden(_, true, true), EvenWGardensInOneDiamond),
    aggregate_all(count, diamond_walled_garden(_, false, true), UnevenWGardensInOneDiamond),
    aggregate_all(count, diamond_walled_garden(_, true, false), EvenWGardensOutsideOneDiamond),
    aggregate_all(count, diamond_walled_garden(_, false, false), UnevenWGardensOutsideOneDiamond),

    aggregate_all(count,
        (diamond_rock(Coord, true, false), (is_in_quadrant(Coord, top_left); is_in_quadrant(Coord, bottom_right))),
        EvenRocksOutsideOneDiamondParity),
    aggregate_all(count,
        (diamond_rock(Coord, false, false), (is_in_quadrant(Coord, top_right); is_in_quadrant(Coord, bottom_left))),
        UnevenRocksOutsideOneDiamondParity),
    aggregate_all(count,
        (diamond_walled_garden(Coord, true, false), (is_in_quadrant(Coord, top_left); is_in_quadrant(Coord, bottom_right))),
        EvenWGardensOutsideOneDiamondParity),
    aggregate_all(count,
        (diamond_walled_garden(Coord, false, false), (is_in_quadrant(Coord, top_right); is_in_quadrant(Coord, bottom_left))),
        UnevenWGardensOutsideOneDiamondParity),

    write(" $$$$$$ "),
    write(EvenRocksOutsideOneDiamondParity),
    write(" "),
    write(UnevenRocksOutsideOneDiamondParity),
    write(" "),
    write(EvenWGardensOutsideOneDiamondParity),
    write(" "),
    write(UnevenWGardensOutsideOneDiamondParity),
    write("\n"),


    % The area of the diamond-shaped tile grid contains an SxS square of smaller diamonds
    garden_dimensions(Width, _),
    StepsToTiles is Steps div Width,
    SquareSide is StepsToTiles * 2 + 1,
    NrCornerTiles is floor(Width * Width / 2),
    NrCornerTilesParity is (SquareSide - 1) / 2,

    Sf is floor(SquareSide / 2),
    Sc is ceil(SquareSide / 2),
    DiamondCountEven   is Sc * Sc + Sf * Sf,
    DiamondCountUneven is Sf * Sc + Sc * Sf,
    % TotalObstacles is
    %     (EvenRocksInOneDiamond + EvenWGardensInOneDiamond) * DiamondCountEven +
    %     (UnevenRocksOutsideOneDiamond + UnevenWGardensOutsideOneDiamond) * DiamondCountUneven,

    % Compute number of reachable cells
    reachable_surface(Steps, RocklessSurface),
    reachable_surface(StepsToTiles,     NrEvenTiles),
    reachable_surface(StepsToTiles - 1, NrUnevenTiles),

    TotalObstacles is
        (EvenRocksInOneDiamond + EvenWGardensInOneDiamond) * NrEvenTiles +
        (UnevenRocksInOneDiamond + UnevenWGardensInOneDiamond) * NrUnevenTiles +
        (EvenRocksOutsideOneDiamond + UnevenRocksOutsideOneDiamond +
         EvenWGardensOutsideOneDiamond + UnevenWGardensOutsideOneDiamond) * (NrCornerTiles - NrCornerTilesParity) +
        (EvenRocksOutsideOneDiamondParity + UnevenRocksOutsideOneDiamondParity +
         EvenWGardensOutsideOneDiamondParity + UnevenWGardensOutsideOneDiamondParity) * NrCornerTilesParity,

    write(" ####### "),
    write(RocklessSurface),
    write(" "),
    write(NrEvenTiles),
    write(" "),
    write(NrUnevenTiles),
    write(" "),
    write(TotalObstacles),
    write("\n"),


    Surface is RocklessSurface - TotalObstacles,


    % ALL THE DIAMONDS AT AN EVEN DIST/PARITY ARE TRUE INNER DIAMONDS,
    % THE OTHER DIAMONDS ARE MADE UP OF OUTER QUADRANTS OF ADJACENT SQUARES

    write(EvenRocksInOneDiamond),
    write("\n"),
    write(EvenWGardensInOneDiamond),
    write("\n"),
    write(UnevenRocksOutsideOneDiamond),
    write("\n"),
    write(UnevenWGardensOutsideOneDiamond),
    write("\n")
    .



corners_surface(RequireEvenSurface, Surface) :-
    % The input SHOULD be a square, so this dimensions unification should never fail
    garden_dimensions(Width, Width),

    % Compute the obstacles that (i.e. rocks and walled gardens) that would otherwise be reachable gardens
    aggregate_all(count, diamond_rock(_, RequireEvenSurface, false), CornerRocks),
    aggregate_all(count, diamond_walled_garden(_, RequireEvenSurface, false), CornerWGardens),

    Obstacles is CornerRocks + CornerWGardens,

    % The corners are the top-left, top-right, bottom-left and bottom-right sections
    % in the input that are separated from the inner diamond by a rock-free zone.
    % The surface contributed by the corners is the same as that contributed by the
    % entire input tile minus that contributed by the center diamond.
    NrEvenCells   is ceil(Width * Width / 2),
    NrUnevenCells is floor(Width * Width / 2),
    (RequireEvenSurface -> TileSurface is NrEvenCells;
                           TileSurface is NrUnevenCells),
    (RequireEvenSurface -> DiamondRadiusSteps is floor(Width / 2) - 1;
                           DiamondRadiusSteps is floor(Width / 2)),
    reachable_surface(DiamondRadiusSteps, DiamondSurface),

    Surface is TileSurface - DiamondSurface - Obstacles,

    write(" Corners Surface "),
    write(DiamondRadiusSteps),
    write(" "),
    write(TileSurface),
    write(" "),
    write(DiamondSurface),
    write(" "),
    write(CornerRocks),
    write(" "),
    write(CornerWGardens),
    write(" "),
    write(Obstacles),
    write("\n").

entire_surface(RequireEvenSurface, Surface) :-
    % The input SHOULD be a square, so this dimensions unification should never fail
    garden_dimensions(Width, Width),

    % Find the obstacles (i.e. rocks and walled gardens) that would otherwise be reachable gardens
    aggregate_all(count, diamond_rock(_, RequireEvenSurface, true), DiamondRocks),
    aggregate_all(count, diamond_rock(_, RequireEvenSurface, false), CornerRocks),
    aggregate_all(count, diamond_walled_garden(_, RequireEvenSurface, true), DiamondWGardens),
    aggregate_all(count, diamond_walled_garden(_, RequireEvenSurface, false), CornerWGardens),

    Obstacles is DiamondRocks + CornerRocks + DiamondWGardens + CornerWGardens,

    % oxo   In the given 3x3 grid, an even column is one that contains one o's than
    % xox   it does x's. The inverse is true for an uneven column. It always holds that
    % oxo   there are either there are either as many even as uneven columns or there
    %       is one more even than uneven columns. So the number of even vs uneven cells
    %       differs by at most one, given any SxS square dimension S.
    NrEvenCells   is ceil(Width * Width / 2),
    NrUnevenCells is floor(Width * Width / 2),
    (RequireEvenSurface -> TileSurface is NrEvenCells;
                           TileSurface is NrUnevenCells),

    Surface is TileSurface - Obstacles,

    write(" Entire Surface "),
    write(DiamondRocks),
    write(" "),
    write(CornerRocks),
    write(" "),
    write(DiamondWGardens),
    write(" "),
    write(CornerWGardens),
    write(" "),
    write(Obstacles),
    write("\n").









diamond_obstacles(RequireEvenSurface, Obstacles) :-
    % Compute the obstacles that (i.e. rocks and walled gardens) that would otherwise be reachable gardens
    aggregate_all(count, diamond_rock(_, RequireEvenSurface, true), DiamondRocks),
    aggregate_all(count, diamond_walled_garden(_, RequireEvenSurface, true), DiamondWGardens),

    Obstacles is DiamondRocks + DiamondWGardens.


corners_obstacles(RequireEvenSurface, Obstacles) :-
    % Compute the obstacles that (i.e. rocks and walled gardens) that would otherwise be reachable gardens
    aggregate_all(count, diamond_rock(_, RequireEvenSurface, false), CornerRocks),
    aggregate_all(count, diamond_walled_garden(_, RequireEvenSurface, false), CornerWGardens),

    Obstacles is CornerRocks + CornerWGardens.


solution2(Steps, Surface) :-
    % Compute number of reachable tiles at even AND uneven distance from the start input tile
    garden_dimensions(Width, _),
    StepsToTiles is Steps div Width,
    reachable_surface(StepsToTiles,     NrEvenTiles),
    reachable_surface(StepsToTiles - 1, NrUnevenTiles),

    % Make assumptions based on MY problem input; paper-and-pencil-approach:
    uneven(Steps),      % The nr of steps is of the form: (steps mod width) + (steps div width)
    even(StepsToTiles), % The final tile-band is at an even "distance"/parity

    % Compute rock and walled garden statistics in/out of the inner diamond
    diamond_obstacles(true,  EvenDiamondObstacles),
    diamond_obstacles(false, UnevenDiamondObstacles),
    corners_obstacles(true,  EvenCornerObstacles),
    corners_obstacles(false, UnevenCornerObstacles),
    EvenObstacles is EvenDiamondObstacles + EvenCornerObstacles,
    UnevenObstacles is UnevenDiamondObstacles + UnevenCornerObstacles,

    SquareSide is StepsToTiles * 2 + 1,

    NrEvenCells   is ceil(Width * Width / 2),
    NrUnevenCells is floor(Width * Width / 2),
    CompleteSurface is 4 * Steps * (Steps + 1) / 2 + 1,

    Surface is
        CompleteSurface -
        NrEvenTiles * NrUnevenCells -
        NrUnevenTiles * NrEvenCells -
        NrEvenTiles * EvenObstacles -
        NrUnevenTiles * UnevenObstacles -
        SquareSide * EvenCornerObstacles +
        (SquareSide - 1) * UnevenCornerObstacles,

    write(CompleteSurface),
    write(" "),
    write(NrEvenTiles * NrUnevenCells),
    write(" "),
    write(NrUnevenTiles * NrEvenCells),
    write(" "),
    write(NrEvenTiles * EvenObstacles),
    write(" "),
    write(NrUnevenTiles * UnevenObstacles),
    write(" "),
    write(SquareSide * EvenCornerObstacles),
    write(" "),
    write((SquareSide - 1) * UnevenCornerObstacles),
    write("\n"),


    write(EvenDiamondObstacles),
    write(" "),
    write(UnevenDiamondObstacles),
    write(" "),
    write(EvenCornerObstacles),
    write(" "),
    write(UnevenCornerObstacles),
    write(" ")

    .


solution3(Steps, Surface) :-
    % Compute number of reachable tiles at even AND uneven distance from the start input tile
    garden_dimensions(Width, Width),
    StepsToTiles is Steps div Width,
    reachable_surface(StepsToTiles,     NrEvenTiles),
    reachable_surface(StepsToTiles - 1, NrUnevenTiles),
    DiamondSide is StepsToTiles + 1,

    % Make assumptions based on MY problem input; paper-and-pencil-approach:
    % uneven(Steps),      % The nr of steps is of the form: (steps mod width) + (steps div width)
    % even(StepsToTiles), % The final tile-band is at an even "distance"/parity

    % Compute the surface (nr of cells) contributions of each type of tile
    entire_surface(true,   TileEvenSurface),
    entire_surface(false,  TileUnevenSurface),
    TiledSurface is NrEvenTiles * TileUnevenSurface + NrUnevenTiles * TileEvenSurface,

    corners_surface(true,  CornerEvenSurface),
    corners_surface(false, CornerUnevenSurface),
    Surface is TiledSurface - DiamondSide * CornerUnevenSurface + (DiamondSide - 1) * CornerEvenSurface,
    

    write(" Sol 3"),
    write(" "),
    write(Steps),
    write(" "),
    write(StepsToTiles),
    write(" "),
    write(NrEvenTiles),
    write(" "),
    write(TileEvenSurface),
    write(" "),
    write(NrUnevenTiles),
    write(" "),
    write(TileUnevenSurface),
    write(" "),
    write(DiamondSide),
    write(" "),
    write(CornerEvenSurface),
    write(" "),
    write(CornerUnevenSurface),
    write("\n")
    .
