OUT_PATH="sierpinski.svg"
N=${1:-3}

if ! [[ $N =~ ^[0-9]+$ ]]; then
    echo "The sole, optional parameter MUST be an unsigned integer, got '$N'";
    exit 1;
fi

swipl -s abc -g "
% Generate a^n b^n c^n pattern
N is $N,
phrase(abc(N), Pattern),
concat_atom(Pattern, PatternAtom),

format('
== a^n b^n c^n generation ==
N: ~w
Generated pattern: ~w
', [N, PatternAtom]),

halt.
"