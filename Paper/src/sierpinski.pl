% Each line in the drawn pattern is registered as a fact
:- dynamic(line/2).

% A point can be either of the two points of any line/2
point(Point) :- line(Point, _); line(_, Point).

% Compute the bounding box of all points, as four
% distinct max and min values. The point (Xmax, Ymax)
% forms the "top right" point and (Xmin, Ymin) the
% "bottom right" point of the bounding box.
bounds(Xmax, Xmin, Ymax, Ymin) :-
  point(Xmax-_), forall(point(Xo-_), Xmax >= Xo),
  point(Xmin-_), forall(point(Xo-_), Xmin =< Xo),
  point(_-Ymax), forall(point(_-Yo), Ymax >= Yo),
  point(_-Ymin), forall(point(_-Yo), Ymin =< Yo).



%%%%%%%%%%%%
% DCG CODE %
%%%%%%%%%%%%

min --> [-].
pls --> [+].

% Parse or generate a sierpinski sequence of
% the specified recursion depth.
sierpinski(N) --> f(N), min, g(N), min, g(N).
f(0) --> [f], !.
f(N) --> {M is N - 1},
         f(M), min, g(M), pls, f(M), pls, g(M), min, f(M).
g(0) --> [g], !.
g(N) --> {M is N - 1},
         g(M), g(M).

% Parse a given sierpinski sequence into a line/2 facts 
sierpinski_svg --> lines(pi/2, 0-0).

lines(Angle, X1-Y1) -->
  ([f] | [g]),
  { X2 is X1 + cos(Angle) * 10, Y2 is Y1 + sin(Angle) * 10 },
  { assertz(line(X1-Y1, X2-Y2)) },
  lines(Angle, X2-Y2).

lines(Angle, Position) -->
  { AngleDelta is 2 * pi / 3 },
  (
    pls, { AngleNew is Angle + AngleDelta } |
    min, { AngleNew is Angle - AngleDelta}
  ),
  lines(AngleNew, Position).

lines(_, _) --> [], !.



%%%%%%%%%%%%%
% CODE CODE %
%%%%%%%%%%%%%

% Dump the current line/2 facts as a SVG image file
write_svg(OutPathAtom) :-
  open(OutPathAtom, write, Out),
  % Compute SVG image bounds + padding
  bounds(Xmax, Xmin, Ymax, Ymin), !,
  Width is  abs(Xmax - Xmin),
  Height is abs(Ymax - Ymin),
  Padding is min(Width / 10, Height / 10),
  WPad is Width + Padding,
  HPad is Height + Padding,
  % Write SVG contents
  format(Out, '<svg width="~w" height="~w">\n', [WPad, HPad]),
  forall(line(X1-Y1, X2-Y2),
         format(Out, '  <line x1="~w" y1="~w" x2="~w" y2="~w" stroke="black" stroke-width="1"/>\n',
                [X1, Y1, X2, Y2])),
  write(Out, '</svg>'),
  % Cleanup
  close(Out).
