OUT_PATH="sierpinski.svg"
REC_DEPTH=${1:-6}

if ! [[ $REC_DEPTH =~ ^[0-9]+$ ]]; then
    echo "The sole, optional parameter MUST be an unsigned integer, got '$REC_DEPTH'";
    exit 1;
fi

swipl -s sierpinski -g "
% Generate sierpinski pattern
RecursionDepth is $REC_DEPTH,
phrase(sierpinski(RecursionDepth), Pattern),
concat_atom(Pattern, PatternAtom),

format('
== Sierpinski SVG generation ==
Recursion depth:   ~w
Generated pattern: ~w
', [RecursionDepth, PatternAtom]),

% Register all generated lines as facts
retractall(line(_, _)),
phrase(sierpinski_svg, Pattern),

% Dump the lines as an SVG image
write_svg('$OUT_PATH'),

halt.
"