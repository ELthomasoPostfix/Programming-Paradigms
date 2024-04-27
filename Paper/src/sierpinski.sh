OUT_PATH="sierpinski.svg"
REC_DEPTH=${1:-6}
USE_DCG_IMPLEMENTATION=${2:-true}

if ! [[ $REC_DEPTH =~ ^[0-9]+$ ]]; then
    echo "The first, optional parameter MUST be an unsigned integer, got '$REC_DEPTH'";
    exit 1;
fi

if ! [[ $USE_DCG_IMPLEMENTATION =~ ^(true)|(false)+$ ]]; then
    echo "The second, optional parameter MUST be 'true' or 'false', got '$USE_DCG_IMPLEMENTATION'";
    exit 1;
fi

swipl -s sierpinski -g "
% Generate sierpinski pattern
RecursionDepth is $REC_DEPTH,
(
    $USE_DCG_IMPLEMENTATION ->
        % Call the pure DCG implementation of sierpinski
        phrase(sierpinski(RecursionDepth), Pattern);

        % Call the partial DCG, partial predicate implementation of sierpinski
        succ(RecursionDepth, RDnxt), call_nth(sierpinski_rec(Pattern), RDnxt)
),
concat_atom(Pattern, PatternAtom),

($USE_DCG_IMPLEMENTATION -> MethodName = 'DCG'; MethodName = 'DCG+predicate'),
format('
== Sierpinski SVG generation ==
Sierpinski impl.:  ~w
Recursion depth:   ~w
Generated pattern: ~w
', [MethodName, RecursionDepth, PatternAtom]),

% Register all generated lines as facts
retractall(line(_, _)),
phrase(sierpinski_svg, Pattern),

% Dump the lines as an SVG image
write_svg('$OUT_PATH'),

halt.
"