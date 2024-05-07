OUT_PATH="sierpinski_completed.svg"
FRAGMENT=${1:-"f-g+f+g-f+gg-f-g+f+g-f-gggg+f-g+f+g-f-gg+f-g+f+g-f+gg-f-g+f+g-f+gggg-f-g+f+g-f-gg+f-g+f+g-f+gg-f-g+f+g-f-gggggggg+f-g+f+g-f-gg+f-g+f+g-f+gg-f-g+f+g-f-gggg+f-g+f+g-f-gg+f-g+f+g-f+gg-f-g+f+g-f+gggg-f-g+"}

if ! [[ $FRAGMENT =~ ^[fg+-]+$ ]]; then
    echo "The first, optional parameter MUST be any (possibly invalid) sierpinski pattern fragment (i.e. a sequence of the characters 'f', 'g', '+' and '-'), got '$Fragment'";
    exit 1;
fi



swipl -s sierpinski -g "
% Load ...//1 DC rule
consult(lambda_calculus),

% Ensure arguments sufficiently instantiated
between(0, 10, Depth),
phrase(sierpinski(Depth), Pattern),

% Define some valid fragment of a sierpinski pattern
Fragment = \"$FRAGMENT\",

% Match the containing pattern
phrase((...(Prefix), Fragment, ...(Postfix)), Pattern),

% Output
concat_atom(Fragment, FragAt),
concat_atom(Pattern, PatAt),
format('
Start Fragment:    ~w
Recursion depth:   ~w
Generated pattern: ~w
image dump: ~w
', [FragAt, Depth, PatAt, $OUT_PATH]),

% Register all generated lines as facts
retractall(line(_, _)),
phrase(sierpinski_svg, Pattern),

% Dump the lines as an SVG image
write_svg('$OUT_PATH'),

halt.
"