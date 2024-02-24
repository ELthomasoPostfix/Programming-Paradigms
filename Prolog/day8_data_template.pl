% The initial L-R string of moves; wrap around if no more moves left.
% The initial string, represented by '< MOVE_PATTERN_TARGET >', should be
% substituted by its true value.
next_moves([], <MOVE_PATTERN_TARGET>).
% Pop the frontmost move from the 'string' of moves.
next_moves([_|Rest], Rest).

:- discontiguous right/2.
:- discontiguous left/2.

