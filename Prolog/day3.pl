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
    num_coordinates(Number, (Xstart, Xend, Y)),
    contains_mark(_, Xstart, Xend, Y).


% Check if the rectangle defined by the points (Xstart-1, Y-1) and (Xend+1, Y+1)
% contains any marking character.
contains_mark(Mark, Xstart, Xend, Y) :-
    mark_coordinates(Mark, (Xmark, Ymark)),
    (eval(Xstart - 1) =< Xmark), (Xmark =< eval(Xend + 1)),
    (eval(Y - 1)      =< Ymark), (Ymark =< eval(Y + 1)),
    % Explicitly CUT because only the first neighbouring mark should
    % result in this predicate resolving to true, no further marks
    % need to be checked.
    !.


gear_ration_sum(Sum) :- Sum is 0.



