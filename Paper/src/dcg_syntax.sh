Word=${1:-'a'}

if ! [[ $Word =~ ^[a-zA-Z][0-9a-zA-Z]*$ ]]; then
    echo "The sole, optional parameter MUST be a valid prolog unquoted atom or variable, got '$Word'";
    exit 1;
fi

swipl -s dcg_syntax -g "
% Call with X as a free variable
phrase(head(X), PatternFree),
% Call with X as a fixed variable;
% this bypasses the cut's defaulting to 'some'
phrase(head('$Word'), PatternChosen),
% Call with an unnamed definite clause
phrase( ( head(X), punct ), PatternMultiPunct),

format('
== example output ==
X free:
    ~w
X chosen (\'$Word\'):
    ~w
DC unnamed:
    ~w
', [PatternFree, PatternChosen, PatternMultiPunct]),

halt.
"