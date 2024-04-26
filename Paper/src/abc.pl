%%%%%%%%%%%%
% DCG CODE %
%%%%%%%%%%%%

abc(N) --> as(N), bs(N), cs(N).
as(0) --> [], !.
as(N) --> [a], { M is N - 1 }, as(M).
bs(0) --> [], !.
bs(N) --> [b], { M is N - 1 }, bs(M).
cs(0) --> [], !.
cs(N) --> [c], { M is N - 1 }, cs(M).
