OUT_PATH="sierpinski_completed.svg"
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
% Load ...//1 DC rule
consult(lambda_calculus),

% Ensure arguments sufficiently instantiated
between(0, 10, Depth),
phrase(sierpinski(Depth), Pattern),

% Define some valid fragment of a sierpinski pattern
Fragment = \"g+f+g-f-g\",

% Match the containing pattern
phrase((...(Prefix), Fragment, ...(Postfix)), Pattern),

% Output
concat_atom(Fragment, FragAt),
concat_atom(Pattern, PatAt),
format('
Start Fragment:    ~w
Recursion depth:   ~w
Generated pattern: ~w
', [FragAt, Depth, PatAt])

% Register all generated lines as facts
retractall(line(_, _)),
phrase(sierpinski_svg, Pattern),

% Dump the lines as an SVG image
write_svg('$OUT_PATH'),

halt.
"