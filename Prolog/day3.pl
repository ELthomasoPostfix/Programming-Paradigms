%%%%%%%%%%%%%%%%
% PARSING CODE %
%%%%%%%%%%%%%%%%

% Parse the input and establish establish the grid coordinates for each marking and number.
:- dynamic(num_coordinates/2).
:- dynamic(mark_coordinates/2).
:- dynamic(line_nr/1).


reset_line_nr :-
    retractall(line_nr(_)),
    assertz(line_nr(0)).

increment_line_nr(NewLineNr) :-
    line_nr(OldLineNr),
    NewLineNr is OldLineNr + 1,
    retractall(line_nr(_)),
    assertz(line_nr(NewLineNr)).


% Dynamically asserts facts (movement pattern, L-R relations) based on the input file.
% Returns false event if it succeeds.
generate_facts(File) :-
    reset_line_nr(),
    % Do file parsing
    file_line(File, Line),
    writeln(Line),
    atom_chars(Line, Chars),
    increment_line_nr(LineCount),
    interpret_line(Chars, LineCount),
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

interpret_line(Line, LineNr) :-
    phrase(num_list(0, LineNr, NumsCoords, MarksCoords), Line),
    % Explicitly CUT because only the first solution should be the
    % maximally greedy parse of the input, which suffices.
    !,
    % Assert numbers and markers in grid as facts.
    foldl(assert_number_coordinates,  NumsCoords, _, _),
    foldl(assert_marking_coordinates, MarksCoords, _, _).

% A foldl accumulator to assert number coordinates as facts.
assert_number_coordinates((Number, Xstart, Xend, Y), _, _) :-
    assertz(num_coordinates(Number, (Xstart, Xend, Y))).

% A foldl accumulator to assert marker coordinates as facts.
assert_marking_coordinates((Mark, Xmark, Ymark), _, _) :-
    assertz(mark_coordinates(Mark, (Xmark, Ymark))).



%%%%%%%%%%%%
% DCG CODE %
%%%%%%%%%%%%

% Make strings valid inputs for the phrase/2 predicate
:- set_prolog_flag(double_quotes, chars).


digit(Digit) --> [Digit], { is_digit(Digit) }.
num([Digit | PartialNumber])  --> digit(Digit), num(PartialNumber) | digit(Digit), { PartialNumber = [] }.

dot --> ".".
whitespace(0) --> "".
whitespace(WSLength) --> dot, whitespace(WSLengthPartial), { WSLength is WSLengthPartial + 1 } | dot, { WSLength is 1 }.

% A list of numbers and markings, separated by whitespace ('.' characters are treated as whitespace, not ' ').
% Recursive case: num_list = whitespace num num_list
num_list(Xcurrent, Ycurrent, [(NumberAtom, Xstart, Xend, Ycurrent) | NumsCoordsPartial], MarksCoords) -->
    whitespace(WSLength), num(Number),

    { Xstart is Xcurrent + WSLength, length(Number, NumLength), Xend is Xstart + NumLength - 1, Xnext is Xend + 1 },
    { atomic_list_concat(Number, NumberAtom) },
    num_list(Xnext, Ycurrent, NumsCoordsPartial, MarksCoords).
% Recursive case: num_list = whitespace marking num_list
num_list(Xcurrent, Ycurrent, NumsCoords, [(Marking, Xmark, Ycurrent) | MarksCoordsPartial]) -->
    whitespace(WSLength), [Marking],

    { Marking \= '.' },
    { Xmark is Xcurrent + WSLength, Xnext is Xmark + 1 },
    num_list(Xnext, Ycurrent, NumsCoords, MarksCoordsPartial).
% Base case: num_list = whitespace
% Ignore trailing whitespace, BUT only after testing all other cases
num_list(_, _, [], []) -->
    whitespace(_).






% Get the sum of all marked numbers.
marked_sum(Sum) :-
    findall(Num, (marked_num(NumAtom), atom_number(NumAtom, Num)), MarkedNums),
    foldl(plus, MarkedNums, 0, Sum).


% Find numbers neighboured by any mark at all.
marked_num(Number) :-
    num_coordinates(Number, NumCoords),
    contains_mark(_, _, NumCoords).


% Check if the rectangle defined by the points (Xstart-1, Y-1) and (Xend+1, Y+1)
% contains any marking character.
% Params:
%       1) Mark atom
%       2) Mark coordinates as a tuple (X, Y)
%       3) The number range around which to look for the mark as (Xstart, Xend, Y)
contains_mark(Mark, (Xmark, Ymark), (Xstart, Xend, Y)) :-
    mark_coordinates(Mark, (Xmark, Ymark)),
    (eval(Xstart - 1) =< Xmark), (Xmark =< eval(Xend + 1)),
    (eval(Y - 1)      =< Ymark), (Ymark =< eval(Y + 1)),
    % Explicitly CUT because only the first neighbouring mark should
    % result in this predicate resolving to true, no further marks
    % need to be checked.
    !.


% Get the sum of all gear ratios.
gear_ratio_sum(Sum) :-
    findall(Ratio, gear_ratio(Ratio), GearRatios),
    foldl(plus, GearRatios, 0, SymmetricSum),
    % The `gear_ratio` predicate finds the ratio for both
    % (Num1=a, Num2=b) AND (Num1=b, Num2=a), so divide by two.
    Sum is SymmetricSum div 2.


% Find the gear ratio of '*' marks neighboured by exatly two numbers.
gear_ratio(Ratio) :-
    % Gather values
    num_coordinates(Number1, NumCoords1),
    num_coordinates(Number2, NumCoords2),
    % Check requirements:
    %   1) Number1 != Number2
    %   2) The two numbers are next to te same '*', i.e. the same MarkCoords
    %   3) There does not exist a Number3 that is also next to the same '*'/MarkCoords
    NumCoords1 \= NumCoords2,
    contains_mark('*', MarkCoords, NumCoords1),
    contains_mark('*', MarkCoords, NumCoords2),
    not(( num_coordinates(_, NumCoords3),
          NumCoords1 \= NumCoords3, NumCoords2 \= NumCoords3,
          contains_mark('*', MarkCoords, NumCoords3)
    )),
    % Compute results
    atom_number(Number1, NumInt1),
    atom_number(Number2, NumInt2),
    Ratio is NumInt1 * NumInt2.

